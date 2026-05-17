library(shiny)
library(shinyjs)

# couleur d'affichage
CYAN <- "#22D3EE"; ROSE <- "#F472B6"; GOLD <- "#C6A037"
`%+%` <- paste0
`%||%` <- function(a, b) if (!is.null(a)) a else b

#  Catalogue des tests
CATEGORIES <- list(
  list(id="moyennes", icon="μ", label="Comparaison de Moyennes",
       desc="Student · Normale · ANOVA · Kruskal-Wallis · Mann-Whitney", color=CYAN,
       tests=list(
         list(id="ttest_small", label="2 moyennes — petits échantillons", desc="Student (variances supposées égales)"),
         list(id="ztest_large", label="2 moyennes — grands échantillons", desc="Loi Normale (Z-test, n≥30)"),
         list(id="anova1",      label="ANOVA à 1 facteur",                desc="k groupes (k≥3) — paramétrique"),
         list(id="kruskal",     label="Kruskal-Wallis",                   desc="k groupes (k≥3) — non paramétrique"),
         list(id="mannwhit",    label="Mann-Whitney",                     desc="2 groupes indép. — non paramétrique"),
         list(id="ttest1",      label="Moy. observée vs théorique",       desc="Test t/Z — 1 échantillon")
       )),
  list(id="proportions", icon="%", label="Comparaison de Proportions",
       desc="Z-test · Chi-deux d'ajustement", color=ROSE,
       tests=list(
         list(id="prop2",       label="2 proportions",            desc="Test Z — 2 proportions indépendantes"),
         list(id="prop1",       label="Proportion obs. vs p₀",    desc="Test Z — proportion vs valeur théorique"),
         list(id="chisq_kprop", label="k proportions (Chi-deux)", desc="Chi-deux d'ajustement, k≥2 groupes")
       )),
  list(id="variances", icon="σ²", label="Comparaison de Variances",
       desc="Fisher · Levene · Bartlett · ANOVA 2 facteurs", color=GOLD,
       tests=list(
         list(id="fisher_var", label="2 variances (Fisher)",             desc="F-test de Fisher"),
         list(id="var_k",      label="k variances (k≥3)",                desc="Levene · Bartlett · Brown-Forsythe"),
         list(id="anova2",     label="ANOVA 2 facteurs sans réplication", desc="Modèle en blocs randomisés")
       )),
  list(id="independance", icon="⊥", label="Indépendance & Corrélation",
       desc="Chi-deux · Pearson · Spearman · Kendall", color="#a78bfa",
       tests=list(
         list(id="chisq_indep", label="Chi-deux d'indépendance", desc="Tableau de contingence r×c"),
         list(id="pearson",     label="Corrélation de Pearson",  desc="Paramétrique — calcul manuel"),
         list(id="spearman",    label="Corrélation de Spearman", desc="Non paramétrique (rangs)"),
         list(id="kendall",     label="Corrélation de Kendall",  desc="Non paramétrique (concordance)")
       )),
  list(id="regression", icon="↗", label="Régression Linéaire Simple",
       desc="Moindres carrés — droite de régression", color="#34d399",
       tests=list(
         list(id="reg_lin", label="Régression linéaire simple", desc="r · a · b · R²")
       )),
  list(id="appariees", icon="⇄", label="Données Appariées",
       desc="Test de Wilcoxon signé", color="#fb923c",
       tests=list(
         list(id="wilcox", label="Test de Wilcoxon (rangs signés)", desc="Non paramétrique — paires avant/après")
       ))
)

get_cat  <- function(cid) Filter(function(c) c$id == cid, CATEGORIES)[[1]]
get_test <- function(tid) {
  for (cat in CATEGORIES) for (t in cat$tests) if (t$id == tid) return(t)
  NULL
}

#style.css
css <- "
@import url('https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@300;400;600;700&family=JetBrains+Mono:wght@400;600&display=swap');
*{box-sizing:border-box;margin:0;padding:0}
body{background:#0A1628;color:#F0F4FF;font-family:'Space Grotesk',sans-serif;min-height:100vh;
  background-image:radial-gradient(ellipse at 20% 20%,rgba(34,211,238,.07) 0%,transparent 60%),
                   radial-gradient(ellipse at 80% 80%,rgba(244,114,182,.07) 0%,transparent 60%)}
.main-wrap{max-width:820px;margin:0 auto;padding:24px 16px 60px}
.app-header{text-align:center;padding:28px 0 20px;position:relative}
.app-logo{font-family:'JetBrains Mono',monospace;font-size:12px;letter-spacing:3px;color:#22D3EE;text-transform:uppercase;margin-bottom:8px}
.app-title{font-size:clamp(24px,5vw,38px);font-weight:700;color:#F0F4FF;line-height:1.1}
.app-title span{color:#22D3EE}
.app-sub{font-size:12px;color:rgba(240,244,255,.4);margin-top:6px;letter-spacing:1.5px}
.btn-about{position:absolute;top:28px;right:0;background:transparent;color:rgba(240,244,255,.4);border:1px solid rgba(255,255,255,.1);border-radius:8px;padding:6px 14px;font-family:'Space Grotesk',sans-serif;font-size:12px;cursor:pointer;transition:all .2s}
.btn-about:hover{border-color:rgba(34,211,238,.4);color:#22D3EE}
.card{background:rgba(255,255,255,.04);border:1px solid rgba(34,211,238,.18);border-radius:14px;padding:28px 24px;margin-top:8px;backdrop-filter:blur(8px)}
.step-badge{display:inline-block;background:rgba(34,211,238,.12);color:#22D3EE;font-size:10px;font-weight:600;letter-spacing:2px;text-transform:uppercase;padding:3px 10px;border-radius:20px;border:1px solid rgba(34,211,238,.3);margin-bottom:12px}
.card-title{font-size:18px;font-weight:600;color:#F0F4FF;margin-bottom:5px}
.card-desc{font-size:13px;color:rgba(240,244,255,.5);margin-bottom:20px;line-height:1.6}
.form-group label{display:none}
.form-control,input[type='text'],input[type='number'],select{background:rgba(255,255,255,.06)!important;border:1.5px solid rgba(34,211,238,.22)!important;border-radius:9px!important;color:#F0F4FF!important;font-family:'Space Grotesk',sans-serif!important;font-size:14px!important;padding:10px 14px!important;width:100%!important;transition:border-color .2s}
.form-control:focus,input:focus,select:focus{outline:none!important;border-color:#22D3EE!important;box-shadow:0 0 0 3px rgba(34,211,238,.10)!important}
.form-control::placeholder{color:rgba(240,244,255,.25)!important}
select.form-control,select{cursor:pointer;-webkit-appearance:none;background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='10' height='10' viewBox='0 0 10 10'%3E%3Cpath fill='%2322D3EE' d='M5 7L0 2h10z'/%3E%3C/svg%3E\")!important;background-repeat:no-repeat!important;background-position:right 12px center!important;padding-right:34px!important}
select option{background:#0f1e35;color:#F0F4FF}
.btn-primary-custom{background:linear-gradient(135deg,#22D3EE 0%,#0ea5c9 100%);color:#0A1628;border:none;border-radius:9px;padding:12px 26px;font-family:'Space Grotesk',sans-serif;font-size:14px;font-weight:600;cursor:pointer;width:100%;margin-top:14px;transition:opacity .2s,transform .15s}
.btn-primary-custom:hover{opacity:.85;transform:translateY(-1px)}
.btn-secondary-custom{background:transparent;color:rgba(240,244,255,.45);border:1px solid rgba(255,255,255,.1);border-radius:9px;padding:9px 18px;font-family:'Space Grotesk',sans-serif;font-size:12px;cursor:pointer;margin-top:8px;width:100%;transition:all .2s}
.btn-secondary-custom:hover{border-color:rgba(34,211,238,.3);color:#F0F4FF}
.btn-dl a{background:rgba(34,211,238,.1);color:#22D3EE!important;border:1px solid rgba(34,211,238,.3);border-radius:9px;padding:9px 18px;font-size:12px;display:block;text-align:center;text-decoration:none!important;margin-top:8px;transition:all .2s}
.btn-dl a:hover{background:rgba(34,211,238,.2)}
.cat-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(220px,1fr));gap:10px;margin-bottom:18px}
.cat-card{background:rgba(255,255,255,.03);border:1.5px solid rgba(255,255,255,.07);border-radius:12px;padding:16px 14px;cursor:pointer;transition:all .22s;position:relative;overflow:hidden}
.cat-card::before{content:'';position:absolute;top:0;left:0;right:0;height:3px;background:var(--cat-color);opacity:.35;transition:opacity .2s}
.cat-card:hover,.cat-card.selected{border-color:var(--cat-color);background:rgba(255,255,255,.05)}
.cat-card:hover::before,.cat-card.selected::before{opacity:1}
.cat-icon{font-size:22px;font-weight:700;color:var(--cat-color);font-family:'JetBrains Mono',monospace;margin-bottom:6px}
.cat-label{font-size:13px;font-weight:600;color:#F0F4FF;margin-bottom:3px}
.cat-desc{font-size:11px;color:rgba(240,244,255,.3);line-height:1.5}
.test-list{display:flex;flex-direction:column;gap:7px;margin-bottom:18px}
.test-item{background:rgba(255,255,255,.03);border:1.5px solid rgba(255,255,255,.07);border-radius:9px;padding:11px 14px;cursor:pointer;transition:all .18s;display:flex;align-items:center;gap:10px}
.test-item:hover,.test-item.selected{border-color:#22D3EE;background:rgba(34,211,238,.05)}
.test-item-dot{width:7px;height:7px;border-radius:50%;background:rgba(34,211,238,.2);flex-shrink:0;transition:all .2s}
.test-item.selected .test-item-dot{background:#22D3EE;box-shadow:0 0 7px #22D3EE}
.test-item-label{font-size:13px;font-weight:600;color:#F0F4FF}
.test-item-desc{font-size:11px;color:rgba(240,244,255,.3);margin-top:1px}
.breadcrumb{font-size:11px;color:rgba(240,244,255,.3);margin-bottom:14px;display:flex;align-items:center;gap:5px;flex-wrap:wrap}
.breadcrumb .bc-sep{color:rgba(240,244,255,.15)}
.breadcrumb .bc-cur{color:#22D3EE;font-weight:600}
.greeting-banner{background:linear-gradient(135deg,rgba(34,211,238,.09),rgba(244,114,182,.05));border:1px solid rgba(34,211,238,.18);border-radius:10px;padding:12px 18px;margin-bottom:18px;font-size:13px;color:#F0F4FF}
.greeting-banner strong{color:#22D3EE}
.input-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px}
.input-grid-3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:10px}
.input-label{font-size:11px;font-weight:600;letter-spacing:.8px;text-transform:uppercase;color:rgba(240,244,255,.4);margin-bottom:5px;margin-top:10px}
.result-box{background:rgba(10,22,40,.85);border:1.5px solid rgba(34,211,238,.18);border-radius:11px;padding:20px;font-family:'JetBrains Mono',monospace;font-size:12px;color:#F0F4FF;white-space:pre-wrap;line-height:1.9;margin-top:14px;max-height:680px;overflow-y:auto}
.r-sec{color:#22D3EE;font-weight:700;letter-spacing:.5px}
.r-comment{color:rgba(34,211,238,.45)}
.r-value{color:#C6A037;font-weight:600}
.r-warn{color:#F472B6}
.r-ok{color:#4ade80}
.r-h0{color:#a78bfa}
.r-interp{color:#34d399}
.r-cond-ok{color:#4ade80}
.r-cond-warn{color:#fb923c}
.r-stat{color:#f0f4ff;font-size:13px;font-weight:700}
.app-footer{text-align:center;margin-top:40px;font-size:10px;color:rgba(240,244,255,.15);letter-spacing:.8px}
.modal-content{background:#0f1e35!important;border:1px solid rgba(34,211,238,.2)!important;border-radius:14px!important}
.modal-header{border-bottom:1px solid rgba(34,211,238,.15)!important}
.modal-footer{border-top:1px solid rgba(34,211,238,.15)!important}
.modal-title{color:#22D3EE!important;font-family:'Space Grotesk',sans-serif!important}
.modal-body{color:#F0F4FF!important;font-family:'Space Grotesk',sans-serif!important;font-size:13px!important;line-height:1.7!important}
.modal-body h5{color:#C6A037;margin:14px 0 5px}
.shiny-notification{background:#0f1e35!important;border:1px solid rgba(244,114,182,.4)!important;color:#F0F4FF!important;border-radius:9px!important}
@media(max-width:480px){.card{padding:18px 14px}.cat-grid{grid-template-columns:1fr 1fr}.input-grid{grid-template-columns:1fr}.input-grid-3{grid-template-columns:1fr}}
"

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$style(HTML(css))
  ),
  div(class="main-wrap",
      div(class="app-header",
          div(class="app-logo", "DevSolution · Informatique L3 · UDs"),
          h1(class="app-title", tags$span("Dev"), "Lap"),
          div(class="app-sub", "18 TESTS STATISTIQUES · SESSIONS INDEPENDANTES"),
          tags$button("A propos", class="btn-about",
                      onclick="Shiny.setInputValue('btn_about', Math.random())")
      ),
      div(id="step_welcome",
          div(class="card",
              div(class="step-badge", "ETAPE 1 / 4 · IDENTIFICATION"),
              div(class="card-title", "Bienvenue sur DevLap"),
              div(class="card-desc", "Entrez votre prénom pour personnaliser votre session."),
              textInput("nom_input", label=NULL, placeholder="Votre prénom…"),
              tags$button("Continuer →", class="btn-primary-custom",
                          onclick="Shiny.setInputValue('btn_nom', Math.random())")
          )
      ),
      hidden(div(id="step_categorie", div(class="card", uiOutput("categorie_ui")))),
      hidden(div(id="step_test_list", div(class="card", uiOutput("test_list_ui")))),
      hidden(div(id="step_test",
                 div(class="card",
                     div(class="step-badge", "ETAPE 4 / 4 · EXECUTION"),
                     uiOutput("test_ui"),
                     uiOutput("result_ui"),
                     div(class="btn-dl", uiOutput("dl_ui")),
                     tags$button("← Autre test", class="btn-secondary-custom",
                                 onclick="Shiny.setInputValue('btn_back3', Math.random())")
                 )
      )),
      div(class="app-footer", "© DevLap · DevSolution · Informatique L3 · UDs · 2026")
  ),
  tags$script(HTML("
    function selectCat(id){
      document.querySelectorAll('.cat-card').forEach(function(el){el.classList.remove('selected');});
      var el=document.getElementById('cc_'+id); if(el) el.classList.add('selected');
      var inp=document.getElementById('cat_choisie'); if(inp) inp.value=id;
    }
    function selectTestItem(id){
      document.querySelectorAll('.test-item').forEach(function(el){el.classList.remove('selected');});
      var el=document.getElementById('ti_'+id); if(el) el.classList.add('selected');
      var inp=document.getElementById('selected_test_id'); if(inp) inp.value=id;
    }
  "))
)

# SERVEUR
server <- function(input, output, session) {
  
  rv <- reactiveValues(nom="", cat_id=NULL, test_id=NULL, last_run=NULL, result_text="")
  
  # Navigation
  observeEvent(input$btn_nom, {
    nom <- trimws(input$nom_input)
    if (nchar(nom) < 1) { showNotification("Veuillez entrer votre prénom.", type="warning"); return() }
    rv$nom <- nom; hide("step_welcome"); show("step_categorie")
  })
  observeEvent(input$btn_categorie, {
    cid <- input$cat_id_val
    if (is.null(cid) || nchar(trimws(cid)) == 0) { showNotification("Sélectionnez une catégorie.", type="warning"); return() }
    rv$cat_id <- cid; rv$test_id <- NULL; rv$last_run <- NULL
    hide("step_categorie"); show("step_test_list")
  })
  observeEvent(input$btn_test, {
    tid <- input$test_id_val
    if (is.null(tid) || nchar(trimws(tid)) == 0) { showNotification("Sélectionnez un test.", type="warning"); return() }
    rv$test_id <- tid; rv$last_run <- NULL; rv$result_text <- ""
    hide("step_test_list"); show("step_test")
  })
  observeEvent(input$btn_back1, { hide("step_categorie"); show("step_welcome") })
  observeEvent(input$btn_back2, { rv$cat_id <- NULL; rv$last_run <- NULL; hide("step_test_list"); show("step_categorie") })
  observeEvent(input$btn_back3, { rv$test_id <- NULL; rv$last_run <- NULL; rv$result_text <- ""; hide("step_test"); show("step_test_list") })
  observeEvent(input$run_test, { rv$last_run <- rv$test_id })
  
  # ── A propos
  observeEvent(input$btn_about, {
    showModal(modalDialog(
      title = "A propos de  DevLap",
      HTML("
        <h5>L'application DevLap</h5>
        <p><strong> DevLap</strong> est une interface interactive de tests statistiques
        pour les etudiants d'Informatique L3 (DevSolution) de l'Universite de Dschang.
        Elle couvre <strong>18 tests</strong> organises en 6 familles et affiche les
        resultats en 6 sections structurees.</p>
        <h5>Auteur</h5>
        <p>DevSolution · L3 Informatique, UDs<br/>
        Contact : <code>emmanueljuniordequa2@gmail.com · 690 624 051</code></p>
        <h5>References</h5>
        <p>M.A. Onabid — Statistiques et Analyse de Donnees, Dept. Maths-Info, UDs<br/>
        Dagnelie (2013) — Statistique theorique et appliquee, De Boeck<br/>
        R Documentation : package stats (base R)</p>
        <h5>Multi-utilisateurs (deploiement local)</h5>
        <p>Chaque onglet/appareil obtient une <strong>session Shiny isolee</strong>.
        Pour utiliser sur plusieurs telephones simultanement :<br/>
        1. Sur le PC serveur : <code>shiny::runApp(host='0.0.0.0', port=3838)</code><br/>
        2. Tous les appareils se connectent au meme Wi-Fi<br/>
        3. Sur chaque telephone : <code>http://&lt;IP_PC&gt;:3838</code><br/>
        Trouver l'IP du PC : <code>ipconfig</code> (Windows) ou <code>hostname -I</code> (Linux)</p>
        <h5>Version</h5><p> DevLap · Mai 2026 · DevSolution, UDs</p>
      "),
      easyClose=TRUE, footer=modalButton("Fermer")
    ))
  })
  
  # UI Catégories
  output$categorie_ui <- renderUI({
    tagList(
      div(class="greeting-banner", "Bonjour ", tags$strong(rv$nom), " ! Selectionnez une categorie de tests."),
      div(class="step-badge", "ETAPE 2 / 4 · CATEGORIE"),
      div(class="card-title", "Choisir une famille de tests"),
      div(class="card-desc", "6 categories."),
      div(class="cat-grid",
          lapply(CATEGORIES, function(cat) {
            div(class="cat-card", id=paste0("cc_",cat$id), style=sprintf("--cat-color:%s",cat$color),
                onclick=sprintf("selectCat('%s')",cat$id),
                div(class="cat-icon",cat$icon), div(class="cat-label",cat$label), div(class="cat-desc",cat$desc))
          })
      ),
      tags$input(type="hidden", id="cat_choisie"),
      tags$button("Voir les tests →", class="btn-primary-custom",
                  onclick="Shiny.setInputValue('btn_categorie',Math.random());Shiny.setInputValue('cat_id_val',document.getElementById('cat_choisie').value);"),
      tags$button("← Changer de nom", class="btn-secondary-custom",
                  onclick="Shiny.setInputValue('btn_back1',Math.random())")
    )
  })
  
  #  UI Liste des tests
  output$test_list_ui <- renderUI({
    req(rv$cat_id)
    cat_data <- get_cat(rv$cat_id)
    tagList(
      div(class="breadcrumb",
          span(" DevLap"), span(class="bc-sep","›"), span(rv$nom),
          span(class="bc-sep","›"), span(class="bc-cur",cat_data$label)),
      div(class="step-badge","ETAPE 3 / 4 · TEST"),
      div(class="card-title",cat_data$label),
      div(class="card-desc",cat_data$desc),
      div(class="test-list",
          lapply(cat_data$tests, function(t) {
            div(class="test-item", id=paste0("ti_",t$id), onclick=sprintf("selectTestItem('%s')",t$id),
                div(class="test-item-dot"),
                div(div(class="test-item-label",t$label), div(class="test-item-desc",t$desc)))
          })
      ),
      tags$input(type="hidden",id="selected_test_id"),
      tags$button("Lancer ce test →", class="btn-primary-custom",
                  onclick="Shiny.setInputValue('btn_test',Math.random());Shiny.setInputValue('test_id_val',document.getElementById('selected_test_id').value);"),
      tags$button("← Changer de categorie", class="btn-secondary-custom",
                  onclick="Shiny.setInputValue('btn_back2',Math.random())")
    )
  })
  
  # Formulaires dynamiques
  output$anova1_groups_ui <- renderUI({
    k  <- max(3L, min(8L, as.integer(input$an_k %||% 3)))
    ex <- list("12, 14, 13, 15","9, 11, 10, 8","16, 18, 17, 20","20, 22, 21, 19","7, 9, 8, 6","25, 23, 26","5, 4, 6","11, 12, 13")
    tagList(lapply(seq_len(k), function(i)
      tagList(div(class="input-label",paste("Groupe",i)),
              textInput(paste0("an_g",i),NULL,value=if(i<=length(ex)) ex[[i]] else ""))))
  })
  output$kruskal_groups_ui <- renderUI({
    k  <- max(3L, min(8L, as.integer(input$kw_k %||% 3)))
    ex <- list("5, 7, 3, 9, 6","8, 10, 12, 7, 11","15, 18, 14, 17, 16","20, 22, 19","3, 4, 2","25, 28, 23","9, 8, 10","1, 2, 3")
    tagList(lapply(seq_len(k), function(i)
      tagList(div(class="input-label",paste("Groupe",i)),
              textInput(paste0("kw_g",i),NULL,value=if(i<=length(ex)) ex[[i]] else ""))))
  })
  output$kprop_groups_ui <- renderUI({
    k  <- max(2L, min(8L, as.integer(input$ckp_k %||% 3)))
    ex <- c(45L,30L,25L,20L,15L,10L,8L,5L)
    tagList(lapply(seq_len(k), function(i)
      tagList(div(class="input-label",paste("Effectif observé — Groupe",i)),
              numericInput(paste0("ckp_o",i),NULL,value=if(i<=length(ex)) ex[[i]] else 10L,min=0,step=1))))
  })
  output$vark_groups_ui <- renderUI({
    k  <- max(3L, min(8L, as.integer(input$vk_k %||% 3)))
    ex <- list("12, 14, 13, 15, 11","9, 11, 10, 8, 12","16, 18, 17, 20, 19","20, 21, 19","5, 6, 4","25, 28, 23","9, 10, 8","2, 3, 1")
    tagList(lapply(seq_len(k), function(i)
      tagList(div(class="input-label",paste("Groupe",i)),
              textInput(paste0("vk_g",i),NULL,value=if(i<=length(ex)) ex[[i]] else ""))))
  })
  output$chisq_rows_ui <- renderUI({
    r  <- max(2L, min(6L, as.integer(input$ci_nrows %||% 2)))
    ex <- list("30, 20, 10","10, 40, 15","15, 10, 25","5, 8, 12","20, 15, 5","8, 12, 6")
    tagList(lapply(seq_len(r), function(i)
      tagList(div(class="input-label",paste("Ligne",i)),
              textInput(paste0("ci_r",i),NULL,value=if(i<=length(ex)) ex[[i]] else ""))))
  })
  output$anova2_cols_ui <- renderUI({
    nc <- max(2L, min(6L, as.integer(input$an2_ncols %||% 3)))
    ex <- list("12, 14, 13, 15","9, 11, 10, 8","16, 18, 17, 20","20, 22, 19, 21","7, 9, 8","25, 23, 27")
    tagList(lapply(seq_len(nc), function(j)
      tagList(div(class="input-label",paste0("Colonne B",j," (niveaux de A, virgule)")),
              textInput(paste0("an2_c",j),NULL,value=if(j<=length(ex)) ex[[j]] else ""))))
  })
  
  #  UI Formulaire test
  output$test_ui <- renderUI({
    req(rv$test_id)
    tid <- rv$test_id; nom <- rv$nom
    tinfo <- get_test(tid); cinfo <- get_cat(rv$cat_id)
    RUN <- tags$button("▶  Executer le test", class="btn-primary-custom",
                       onclick="Shiny.setInputValue('run_test', Math.random())")
    tagList(
      div(class="breadcrumb",
          span(" DevLap"),span(class="bc-sep","›"),span(nom),
          span(class="bc-sep","›"),span(cinfo$label),
          span(class="bc-sep","›"),span(class="bc-cur",tinfo$label)),
      div(class="card-title",tinfo$label),
      div(class="card-desc",sprintf("Les champs sont pre-remplis avec un exemple, %s. Modifiez puis cliquez Executer.",nom)),
      switch(tid,
             ttest_small = tagList(
               div(class="card-desc","Hypothese : variances egales (modele pooled)."),
               div(class="input-label","Groupe 1"),
               div(class="input-grid-3",
                   div(div(class="input-label","n₁"),numericInput("ts_n1",NULL,12,min=2,step=1)),
                   div(div(class="input-label","x̄₁"),numericInput("ts_xbar1",NULL,24.3,step=.01)),
                   div(div(class="input-label","s₁"),numericInput("ts_s1",NULL,3.2,min=.001,step=.01))),
               div(class="input-label","Groupe 2"),
               div(class="input-grid-3",
                   div(div(class="input-label","n₂"),numericInput("ts_n2",NULL,10,min=2,step=1)),
                   div(div(class="input-label","x̄₂"),numericInput("ts_xbar2",NULL,21.8,step=.01)),
                   div(div(class="input-label","s₂"),numericInput("ts_s2",NULL,2.9,min=.001,step=.01))),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("ts_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("ts_alt",NULL,c("Bilateral (μ₁≠μ₂)"="bilateral","μ₁>μ₂"="droite","μ₁<μ₂"="gauche")))),RUN),
             ztest_large = tagList(
               div(class="input-label","Groupe 1 (n≥30)"),
               div(class="input-grid-3",
                   div(div(class="input-label","n₁"),numericInput("zt_n1",NULL,45,min=30,step=1)),
                   div(div(class="input-label","x̄₁"),numericInput("zt_xbar1",NULL,78.5,step=.01)),
                   div(div(class="input-label","s₁"),numericInput("zt_s1",NULL,8.2,min=.001,step=.01))),
               div(class="input-label","Groupe 2 (n≥30)"),
               div(class="input-grid-3",
                   div(div(class="input-label","n₂"),numericInput("zt_n2",NULL,40,min=30,step=1)),
                   div(div(class="input-label","x̄₂"),numericInput("zt_xbar2",NULL,74.3,step=.01)),
                   div(div(class="input-label","s₂"),numericInput("zt_s2",NULL,7.6,min=.001,step=.01))),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("zt_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("zt_alt",NULL,c("Bilateral (μ₁≠μ₂)"="bilateral","μ₁>μ₂"="droite","μ₁<μ₂"="gauche")))),RUN),
             anova1 = tagList(
               div(class="input-grid",
                   div(div(class="input-label","Nombre de groupes (3-8)"),numericInput("an_k",NULL,3,min=3,max=8,step=1)),
                   div(div(class="input-label","Seuil α"),numericInput("an_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               div(class="input-label","Post-hoc si H₀ rejetee"),
               selectInput("an_posthoc",NULL,c("Tukey HSD"="tukey","Aucun"="none")),
               uiOutput("anova1_groups_ui"),RUN),
             kruskal = tagList(
               div(class="input-grid",
                   div(div(class="input-label","Nombre de groupes (3-8)"),numericInput("kw_k",NULL,3,min=3,max=8,step=1)),
                   div(div(class="input-label","Seuil α"),numericInput("kw_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               uiOutput("kruskal_groups_ui"),RUN),
             mannwhit = tagList(
               div(class="input-label","Groupe A"),textInput("mw_x1",NULL,value="5, 7, 3, 9, 4, 8"),
               div(class="input-label","Groupe B"),textInput("mw_x2",NULL,value="8, 6, 10, 5, 12, 9"),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("mw_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("mw_alt",NULL,c("Bilateral (μA≠μB)"="bilateral","μA>μB"="droite","μA<μB"="gauche")))),RUN),
             ttest1 = tagList(
               div(class="input-grid",
                   div(div(class="input-label","Taille n"),numericInput("tt1_n",NULL,20,min=1,step=1)),
                   div(div(class="input-label","Moy. theorique μ₀"),numericInput("tt1_mu",NULL,50,step=.1))),
               div(class="input-grid",
                   div(div(class="input-label","Moy. observee x̄"),numericInput("tt1_xbar",NULL,53.8,step=.01)),
                   div(div(class="input-label","Ecart-type s"),numericInput("tt1_s",NULL,6.4,min=.001,step=.01))),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("tt1_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("tt1_alt",NULL,c("Bilateral (μ≠μ₀)"="bilateral","μ>μ₀"="droite","μ<μ₀"="gauche")))),RUN),
             prop2 = tagList(
               div(class="input-grid",
                   div(div(class="input-label","Succes k₁"),numericInput("pp2_k1",NULL,45,min=0,step=1)),
                   div(div(class="input-label","Effectif n₁"),numericInput("pp2_n1",NULL,100,min=1,step=1))),
               div(class="input-grid",
                   div(div(class="input-label","Succes k₂"),numericInput("pp2_k2",NULL,38,min=0,step=1)),
                   div(div(class="input-label","Effectif n₂"),numericInput("pp2_n2",NULL,100,min=1,step=1))),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("pp2_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("pp2_alt",NULL,c("Bilateral (p₁≠p₂)"="two.sided","p₁>p₂"="greater","p₁<p₂"="less")))),RUN),
             prop1 = tagList(
               div(class="input-grid",
                   div(div(class="input-label","Succes k"),numericInput("p1_k",NULL,45,min=0,step=1)),
                   div(div(class="input-label","Effectif n"),numericInput("p1_n",NULL,100,min=1,step=1))),
               div(class="input-grid",
                   div(div(class="input-label","Proportion theorique p₀"),numericInput("p1_p0",NULL,.5,min=.001,max=.999,step=.05)),
                   div(div(class="input-label","Seuil α"),numericInput("p1_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               selectInput("p1_alt",NULL,c("Bilateral (p≠p₀)"="two.sided","p>p₀"="greater","p<p₀"="less")),RUN),
             chisq_kprop = tagList(
               div(class="input-grid",
                   div(div(class="input-label","Nombre de groupes (2-8)"),numericInput("ckp_k",NULL,3,min=2,max=8,step=1)),
                   div(div(class="input-label","Seuil α"),numericInput("ckp_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               div(class="input-label","Loi sous H₀"),
               selectInput("ckp_mode",NULL,c("Equirepartition (pi=1/k)"="equal","Probabilites personnalisees"="custom")),
               conditionalPanel("input.ckp_mode=='custom'",
                                div(class="input-label","Probabilites p₁,...,pₖ (somme=1)"),
                                textInput("ckp_probs",NULL,placeholder="ex : 0.50, 0.30, 0.20")),
               uiOutput("kprop_groups_ui"),RUN),
             fisher_var = tagList(
               div(class="input-grid",
                   div(div(class="input-label","n₁"),numericInput("fv_n1",NULL,12,min=2,step=1)),
                   div(div(class="input-label","s₁"),numericInput("fv_s1",NULL,2.4,min=.001,step=.01))),
               div(class="input-grid",
                   div(div(class="input-label","n₂"),numericInput("fv_n2",NULL,10,min=2,step=1)),
                   div(div(class="input-label","s₂"),numericInput("fv_s2",NULL,3.8,min=.001,step=.01))),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("fv_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Type de test"),selectInput("fv_type",NULL,c("Bilateral (σ₁²≠σ₂²)"="bilateral","σ₁²>σ₂²"="droite","σ₁²<σ₂²"="gauche")))),RUN),
             var_k = tagList(
               div(class="input-grid",
                   div(div(class="input-label","Nombre de groupes (3-8)"),numericInput("vk_k",NULL,3,min=3,max=8,step=1)),
                   div(div(class="input-label","Seuil α"),numericInput("vk_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               uiOutput("vark_groups_ui"),RUN),
             anova2 = tagList(
               div(class="card-desc","Chaque colonne = un niveau du facteur B. Meme longueur (= niveaux du facteur A)."),
               div(class="input-grid",
                   div(div(class="input-label","Nb colonnes B (2-6)"),numericInput("an2_ncols",NULL,3,min=2,max=6,step=1)),
                   div(div(class="input-label","Seuil α"),numericInput("an2_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               uiOutput("anova2_cols_ui"),RUN),
             chisq_indep = tagList(
               div(class="card-desc","Entrez chaque ligne du tableau de contingence (virgules). Meme nb de colonnes partout."),
               div(class="input-grid",
                   div(div(class="input-label","Nb lignes r (2-6)"),numericInput("ci_nrows",NULL,2,min=2,max=6,step=1)),
                   div(div(class="input-label","Seuil α"),numericInput("ci_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               div(class="input-label","Correction de Yates (2x2 uniquement)"),
               selectInput("ci_yates",NULL,c("Non"="FALSE","Oui"="TRUE")),
               uiOutput("chisq_rows_ui"),RUN),
             pearson = tagList(
               div(class="input-label","Serie X"),textInput("cor_p_x",NULL,value="2.3, 4.5, 3.1, 6.2, 5.0, 7.8"),
               div(class="input-label","Serie Y"),textInput("cor_p_y",NULL,value="1.8, 4.2, 2.9, 5.8, 4.7, 7.1"),
               div(class="input-grid",
                   div(div(class="input-label","r₀ sous H₀ (generalement 0)"),numericInput("cor_p_r0",NULL,0,min=-.99,max=.99,step=.01)),
                   div(div(class="input-label","Seuil α"),numericInput("cor_p_alpha",NULL,.05,min=.001,max=.20,step=.01))),
               selectInput("cor_p_alt",NULL,c("Bilateral (rp≠r₀)"="bilateral","rp>r₀"="droite","rp<r₀"="gauche")),RUN),
             spearman = tagList(
               div(class="input-label","Serie X"),textInput("cor_s_x",NULL,value="2.3, 4.5, 3.1, 6.2, 5.0, 7.8"),
               div(class="input-label","Serie Y"),textInput("cor_s_y",NULL,value="1.8, 4.2, 2.9, 5.8, 4.7, 7.1"),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("cor_s_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("cor_s_alt",NULL,c("Bilateral (ρ≠0)"="two.sided","ρ>0"="greater","ρ<0"="less")))),RUN),
             kendall = tagList(
               div(class="input-label","Serie X"),textInput("cor_k_x",NULL,value="2.3, 4.5, 3.1, 6.2, 5.0, 7.8"),
               div(class="input-label","Serie Y"),textInput("cor_k_y",NULL,value="1.8, 4.2, 2.9, 5.8, 4.7, 7.1"),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("cor_k_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("cor_k_alt",NULL,c("Bilateral (τ≠0)"="two.sided","τ>0"="greater","τ<0"="less")))),RUN),
             reg_lin = tagList(
               div(class="input-label","Valeurs de X (variable explicative)"),textInput("rl_x",NULL,value="1, 2, 3, 4, 5, 6"),
               div(class="input-label","Valeurs de Y (variable reponse)"),textInput("rl_y",NULL,value="2.5, 4.1, 5.8, 7.2, 9.0, 10.6"),RUN),
             wilcox = tagList(
               div(class="input-label","Mesures AVANT"),textInput("wil_avant",NULL,value="5, 7, 3, 9, 4, 8, 6"),
               div(class="input-label","Mesures APRES"),textInput("wil_apres",NULL,value="8, 9, 5, 11, 7, 10, 9"),
               div(class="input-grid",
                   div(div(class="input-label","Seuil α"),numericInput("wil_alpha",NULL,.05,min=.001,max=.20,step=.01)),
                   div(div(class="input-label","Alternative"),selectInput("wil_alt",NULL,c("Bilateral (avant≠apres)"="two.sided","avant>apres"="greater","avant<apres"="less")))),RUN),
             div(class="r-warn","Test non implemente.")
      )
    )
  })
  
  #  Resultat
  output$result_ui <- renderUI({
    req(rv$last_run, rv$last_run == rv$test_id, input$run_test)
    res <- isolate(compute_test())
    rv$result_text <- gsub("<[^>]+>", "", res)
    div(class="result-box", HTML(res))
  })
  
  output$dl_ui <- renderUI({
    req(nchar(rv$result_text) > 0)
    downloadLink("dl_txt", "Telecharger le resultat (.txt)")
  })
  
  # format de telechargement
  output$dl_txt <- downloadHandler(
    filename = function() paste0(" DevSolution_", rv$test_id, "_", format(Sys.Date(),"%Y%m%d"), ".txt"),
    content  = function(file) writeLines(rv$result_text, file, useBytes=FALSE)
  )
  
  #  Helpers communs
  pv <- function(s, as_int=FALSE) {
    raw <- gsub("[,[:space:]]+", " ", trimws(s))
    v   <- if (as_int) suppressWarnings(as.integer(unlist(strsplit(raw," "))))
    else        suppressWarnings(as.numeric(unlist(strsplit(raw," "))))
    v[!is.na(v)]
  }
  
  # Separateur de section numerotee
  S <- function(n, titre)
    sprintf('\n<span class="r-sec">─── %d. %s ───────────────────────────────</span>', n, titre)
  
  # Ligne de condition (vert=OK, orange=avertissement)
  CL <- function(ok, txt)
    if (ok) sprintf('  <span class="r-cond-ok">✓</span>  %s', txt)
  else    sprintf('  <span class="r-cond-warn">⚠</span>  %s', txt)
  
  # Hypotheses
  HYP <- function(h0, h1) sprintf('<span class="r-h0">  H₀ : %s\n  H₁ : %s</span>', h0, h1)
  
  # Valeur en or
  V <- function(x, fmt="%.4f") sprintf(paste0('<span class="r-value">',fmt,'</span>'), x)
  
  # Statistique observee
  STAT <- function(nom, val)
    sprintf('<span class="r-stat">  %s = %.4f</span>', nom, val)
  
  # ZNR + position de la stat
  ZNR_LINE <- function(znr_str, stat_name, stat_val, alpha)
    paste0("  ZNR = ", V(znr_str,"%s"), "   (α = ", alpha, ")\n",
           STAT(stat_name, stat_val))
  
  # ── DECISION par comparaison stat_obs vs valeur critique
  DEC_Z <- function(appart)
    if (appart) '<span class="r-warn">  ✘  NON-REJET de H₀  (stat. ∈ ZNR)</span>'
  else        '<span class="r-ok">  ✔  REJET de H₀  (stat. ∉ ZNR)</span>'
  
  # Interpretation (vert)
  INTERP <- function(txt) sprintf('<span class="r-interp">  ↳  %s</span>', txt)
  
  # Pied
  FOOT <- function(nom) sprintf('<span class="r-comment">─── Resulat generer pour : %s |  DevLap · Info 3 · UDs - 2026 ───</span>', nom)
  
  # Fonctions ZNR normales / Student
  znr_norm <- function(alpha, alt) {
    if (alt %in% c("bilateral","two.sided")) {
      c_ <- qnorm(1-alpha/2)
      list(str=sprintf("[ %.4f ; %.4f ]",-c_,c_), fn=function(z) abs(z)<=c_, crit=c_)
    } else if (alt %in% c("droite","greater")) {
      c_ <- qnorm(1-alpha)
      list(str=sprintf("] -inf ; %.4f ]",c_), fn=function(z) z<=c_, crit=c_)
    } else {
      c_ <- qnorm(1-alpha)
      list(str=sprintf("[ %.4f ; +inf [", -c_), fn=function(z) z>=-c_, crit=c_)
    }
  }
  znr_stud <- function(alpha, alt, ddl) {
    if (alt %in% c("bilateral","two.sided")) {
      c_ <- qt(1-alpha/2,ddl)
      list(str=sprintf("[ %.4f ; %.4f ]",-c_,c_), fn=function(t) abs(t)<=c_, crit=c_)
    } else if (alt %in% c("droite","greater")) {
      c_ <- qt(1-alpha,ddl)
      list(str=sprintf("] -inf ; %.4f ]",c_), fn=function(t) t<=c_, crit=c_)
    } else {
      c_ <- qt(1-alpha,ddl)
      list(str=sprintf("[ %.4f ; +inf [",-c_), fn=function(t) t>=-c_, crit=c_)
    }
  }
  
  # 18 tests
  compute_test <- reactive({
    req(rv$test_id)
    tid <- rv$test_id; nom <- rv$nom
    tryCatch({
      switch(tid,
             
             # 1 ─ t-test Student 2 indep.
             ttest_small = {
               n1<-input$ts_n1; xb1<-input$ts_xbar1; s1<-input$ts_s1
               n2<-input$ts_n2; xb2<-input$ts_xbar2; s2<-input$ts_s2
               alpha<-input$ts_alpha; alt<-input$ts_alt
               if(is.na(n1)||n1<2) stop("n₁ doit etre un entier >= 2.")
               if(is.na(n2)||n2<2) stop("n₂ doit etre un entier >= 2.")
               if(is.na(s1)||s1<=0) stop("s₁ doit etre strictement positif.")
               if(is.na(s2)||s2<=0) stop("s₂ doit etre strictement positif.")
               Sp <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
               se <- Sp*sqrt(1/n1+1/n2)
               tobs <- (xb1-xb2)/se; ddl <- n1+n2-2
               znr  <- znr_stud(alpha,alt,ddl); ap <- znr$fn(tobs)
               alt_lbl <- if(alt=="bilateral")"bilateral" else if(alt=="droite")"unilateral droit" else "unilateral gauche"
               h0 <- "μ₁ = μ₂"; h1 <- if(alt=="bilateral")"μ₁ ≠ μ₂" else if(alt=="droite")"μ₁ > μ₂" else "μ₁ < μ₂"
               concl <- if(ap) "A ce niveau de signification, on ne peut pas conclure que les deux moyennes sont differentes." else
                 sprintf("Les moyennes sont significativement differentes (μ₁%s%.4f, μ₂%s%.4f).",
                         if(xb1>xb2)">" else "<",xb1,if(xb1>xb2)"<" else ">",xb2)
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  Groupe 1 : n₁=%d   x̄₁=%s   s₁=%s", n1,V(xb1),V(s1)),
                 sprintf("  Groupe 2 : n₂=%d   x̄₂=%s   s₂=%s", n2,V(xb2),V(s2)),
                 sprintf("  Seuil α = %.2f   Test %s", alpha, alt_lbl),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Student t  (ddl = %d)</span>", ddl),
                 CL(n1>=2, sprintf("n₁ = %d >= 2 (taille minimale)", n1)),
                 CL(n2>=2, sprintf("n₂ = %d >= 2 (taille minimale)", n2)),
                 CL(TRUE,  "Normalite des populations supposee"),
                 CL(TRUE,  "Variances supposees egales (modele pooled)"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  Sp (ecart-type poole) = %s", V(Sp)),
                 sprintf("  SE = Sp . sqrt(1/n₁ + 1/n₂) = %s", V(se)),
                 STAT("t_obs = (x̄₁ - x̄₂) / SE", tobs),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr$str,"t_obs",tobs,alpha),
                 sprintf("  Point critique t_crit = %s   (loi t, ddl=%d, α=%.2f)", V(znr$crit), ddl, alpha),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap), INTERP(concl),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 2 ─ Z-test 2 grands echantillons
             ztest_large = {
               n1<-input$zt_n1; xb1<-input$zt_xbar1; s1<-input$zt_s1
               n2<-input$zt_n2; xb2<-input$zt_xbar2; s2<-input$zt_s2
               alpha<-input$zt_alpha; alt<-input$zt_alt
               if(is.na(n1)||n1<30) stop(sprintf("n₁=%d : le Z-test exige n >= 30. Utilisez le t-test pour n<30.",n1%||%0))
               if(is.na(n2)||n2<30) stop(sprintf("n₂=%d : le Z-test exige n >= 30.",n2%||%0))
               if(is.na(s1)||s1<=0) stop("s₁ doit etre strictement positif.")
               if(is.na(s2)||s2<=0) stop("s₂ doit etre strictement positif.")
               se <- sqrt(s1^2/n1+s2^2/n2); Zobs <- (xb1-xb2)/se
               znr <- znr_norm(alpha,alt); ap <- znr$fn(Zobs)
               h0 <- "μ₁ = μ₂"; h1 <- if(alt=="bilateral")"μ₁ ≠ μ₂" else if(alt=="droite")"μ₁ > μ₂" else "μ₁ < μ₂"
               concl <- if(ap) "Pas de difference significative entre les deux moyennes." else "Les deux moyennes sont significativement differentes."
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  Groupe 1 : n₁=%d   x̄₁=%s   s₁=%s", n1,V(xb1),V(s1)),
                 sprintf("  Groupe 2 : n₂=%d   x̄₂=%s   s₂=%s", n2,V(xb2),V(s2)),
                 sprintf("  Seuil α = %.2f", alpha),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 "  Loi : <span class='r-value'>Normale N(0,1)  (grands echantillons)</span>",
                 CL(n1>=30, sprintf("n₁ = %d >= 30", n1)),
                 CL(n2>=30, sprintf("n₂ = %d >= 30", n2)),
                 CL(s1>0, "s₁ > 0"),
                 CL(s2>0, "s₂ > 0"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  SE = sqrt(s₁²/n₁ + s₂²/n₂) = %s", V(se)),
                 STAT("Z_obs = (x̄₁ - x̄₂) / SE", Zobs),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr$str,"Z_obs",Zobs,alpha),
                 sprintf("  z_crit = %s   (N(0,1), α=%.2f)", V(znr$crit), alpha),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap), INTERP(concl),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 3 ─ ANOVA 1 facteur
             anova1 = {
               k     <- max(3L,min(8L,as.integer(input$an_k%||%3)))
               alpha <- input$an_alpha
               grps  <- lapply(seq_len(k), function(i){
                 v <- pv(input[[paste0("an_g",i)]])
                 if(length(v)<2) stop(sprintf("Groupe %d : au moins 2 valeurs requises.",i)); v
               })
               N <- sum(sapply(grps,length))
               y <- unlist(grps); gf <- factor(rep(seq_len(k),sapply(grps,length)))
               res_aov <- aov(y~gf); sm <- summary(res_aov)[[1]]
               Fst<-sm$`F value`[1]
               d1<-sm$Df[1]; d2<-sm$Df[2]
               SSC<-sm$`Sum Sq`[1]; SSE<-sm$`Sum Sq`[2]; SST<-SSC+SSE
               MSC<-sm$`Mean Sq`[1]; MSE<-sm$`Mean Sq`[2]; Fc<-qf(1-alpha,d1,d2)
               ap <- (Fst <= Fc)
               ni <- sapply(grps,length); moy <- sapply(grps,mean)
               gl <- paste(sapply(seq_len(k), function(i)
                 sprintf("  G%d: n=%d  moy=%s  s=%s",i,ni[i],V(moy[i]),V(sd(grps[[i]])))), collapse="\n")
               base <- paste(
                 S(1,"DONNEES SAISIES"), gl,
                 sprintf("  k=%d groupes   N=%d obs.   alpha=%.2f",k,N,alpha),
                 S(2,"FORMULATION DES HYPOTHESES"),
                 HYP("mu1 = mu2 = ... = muk  (toutes les moyennes sont egales)",
                     "Au moins une moyenne differe"),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Fisher F(%d, %d)</span>",d1,d2),
                 CL(k>=3, sprintf("k=%d groupes (minimum 3)",k)),
                 CL(all(sapply(grps,length)>=2), "Chaque groupe a au moins 2 observations"),
                 CL(TRUE,"Normalite des groupes supposee"),
                 CL(TRUE,"Independance des observations supposee"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  SSC=%s  SSE=%s  SST=%s", V(SSC),V(SSE),V(SST)),
                 sprintf("  MSC=%s  MSE=%s", V(MSC),V(MSE)),
                 STAT("F_obs = MSC / MSE", Fst),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  ZNR = [ 0 ; %s ]   (F(%d,%d), alpha=%.2f)", V(Fc),d1,d2,alpha),
                 sprintf("  F_crit = %s", V(Fc)),
                 STAT("F_obs", Fst),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(!ap)"Au moins une paire de moyennes est significativement differente. Effectuez un test post-hoc." else "Les moyennes des groupes ne sont pas significativement differentes."),
                 "", FOOT(nom), sep="\n"
               )
               if(!ap && input$an_posthoc=="tukey"){
                 th <- TukeyHSD(res_aov)$gf
                 tl <- paste(sapply(seq_len(nrow(th)), function(i)
                   sprintf("  %s : diff=%s  %s", rownames(th)[i],V(th[i,1]),
                           if(th[i,4]<alpha)"<span class='r-ok'>sig.</span>" else "<span class='r-warn'>non sig.</span>")),
                   collapse="\n")
                 base <- paste(base, "\n  Post-hoc Tukey HSD :\n", tl, sep="")
               }
               base
             },
             
             # 4 ─ Kruskal-Wallis
             kruskal = {
               k     <- max(3L,min(8L,as.integer(input$kw_k%||%3)))
               alpha <- input$kw_alpha
               grps  <- lapply(seq_len(k), function(i){
                 v <- pv(input[[paste0("kw_g",i)]])
                 if(length(v)<2) stop(sprintf("Groupe %d : au moins 2 valeurs requises.",i)); v
               })
               ni <- sapply(grps,length); N <- sum(ni)
               y  <- unlist(grps); gi <- rep(seq_len(k),ni)
               rg <- rank(y); ri <- tapply(rg,gi,sum)
               H  <- (12/(N*(N+1)))*sum(ri^2/ni)-3*(N+1)
               ddl <- k-1; Xc <- qchisq(1-alpha,df=ddl)
               ap <- (H <= Xc)
               gl <- paste(sapply(seq_len(k), function(i)
                 sprintf("  G%d: n=%d  moy=%s  R=%s",i,ni[i],V(mean(grps[[i]])),V(as.numeric(ri[i])))), collapse="\n")
               paste(
                 S(1,"DONNEES SAISIES"), gl,
                 sprintf("  k=%d groupes   N=%d obs.   alpha=%.2f",k,N,alpha),
                 S(2,"FORMULATION DES HYPOTHESES"),
                 HYP("Les k distributions sont identiques","Au moins une distribution differe"),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Chi-deux  chi2(%d)</span>  (approximation pour N grand)",ddl),
                 CL(k>=3, sprintf("k=%d >= 3 groupes",k)),
                 CL(all(ni>=2), "Chaque groupe a au moins 2 observations"),
                 CL(TRUE,"Distributions continues supposees"),
                 CL(TRUE,"Independance des observations supposee"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 "  H = [12 / (N(N+1))] . Sum(Ri² / ni) - 3(N+1)",
                 STAT("H_obs", H),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  ZNR = [ 0 ; %s ]   (chi2(%d), alpha=%.2f)", V(Xc),ddl,alpha),
                 sprintf("  chi2_crit = %s", V(Xc)),
                 STAT("H_obs", H),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(!ap)"Les distributions ne sont pas toutes identiques. Effectuez des comparaisons par paires." else "Pas de difference significative entre les distributions."),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 5 ─ Mann-Whitney
             mannwhit = {
               x1 <- pv(input$mw_x1); x2 <- pv(input$mw_x2)
               alpha <- input$mw_alpha; alt <- input$mw_alt
               if(length(x1)<2) stop("Groupe A : au moins 2 valeurs.")
               if(length(x2)<2) stop("Groupe B : au moins 2 valeurs.")
               n1<-length(x1); n2<-length(x2)
               W_tot <- (n1+n2)*(n1+n2+1)/2
               rg    <- rank(c(x1,x2)); gv <- c(rep(1,n1),rep(2,n2))
               W1<-sum(rg[gv==1]); W2<-W_tot-W1
               U1<-W1-n1*(n1+1)/2; U2<-W2-n2*(n2+1)/2; Uobs<-min(U1,U2)
               h0 <- "dist.A = dist.B"; h1 <- if(alt=="bilateral")"dist.A != dist.B" else if(alt=="droite")"dist.A > dist.B" else "dist.A < dist.B"
               if(max(n1,n2)<=20){
                 aq  <- if(alt=="bilateral") alpha/2 else alpha
                 Uc  <- qwilcox(aq,n1,n2); ap <- (Uobs>=Uc)
                 znr_s <- if(alt=="bilateral") sprintf("(%d ; %d)",as.integer(Uc),as.integer(W_tot-Uc))
                 else sprintf("[%d ; +inf [",as.integer(Uc))
                 paste(
                   S(1,"DONNEES SAISIES"),
                   sprintf("  Groupe A (n₁=%d) : %s",n1,paste(x1,collapse=", ")),
                   sprintf("  Groupe B (n₂=%d) : %s",n2,paste(x2,collapse=", ")),
                   sprintf("  alpha=%.2f",alpha),
                   S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                   S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                   "  Loi : <span class='r-value'>Table Mann-Whitney (n <= 20)</span>",
                   CL(n1>=2,"n₁ >= 2"), CL(n2>=2,"n₂ >= 2"),
                   CL(TRUE,"Distributions continues supposees"),
                   S(4,"VALEUR OBSERVEE DU TEST"),
                   sprintf("  Rangs totaux : W₁=%s   W₂=%s",V(W1),V(W2)),
                   sprintf("  U₁=%s   U₂=%s",V(U1),V(U2)),
                   STAT("U_obs = min(U1, U2)", Uobs),
                   S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                   sprintf("  ZNR = %s   U_crit=%d   (alpha=%.2f)",znr_s,as.integer(Uc),alpha),
                   STAT("U_obs", Uobs),
                   S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                   INTERP(if(!ap)"Les deux distributions sont significativement differentes." else "Pas de difference significative entre les distributions."),
                   "", FOOT(nom), sep="\n"
                 )
               } else {
                 mu_u <- n1*n2/2; sg_u <- sqrt(n1*n2*(n1+n2+1)/12)
                 Zobs <- (Uobs-mu_u)/sg_u
                 znr  <- znr_norm(alpha,alt); ap <- znr$fn(Zobs)
                 paste(
                   S(1,"DONNEES SAISIES"),
                   sprintf("  n₁=%d   n₂=%d   alpha=%.2f",n1,n2,alpha),
                   S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                   S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                   "  Loi : <span class='r-value'>Normale N(0,1)  (approximation n > 20)</span>",
                   CL(TRUE,"n > 20 : approximation normale valide"),
                   S(4,"VALEUR OBSERVEE DU TEST"),
                   sprintf("  U_obs=%s   mu_U=%s   sigma_U=%s",V(Uobs),V(mu_u),V(sg_u)),
                   STAT("Z_obs = (U - mu_U) / sigma_U", Zobs),
                   S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                   ZNR_LINE(znr$str,"Z_obs",Zobs,alpha),
                   sprintf("  z_crit = %s", V(znr$crit)),
                   S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                   INTERP(if(!ap)"Difference significative entre les distributions." else "Pas de difference significative."),
                   "", FOOT(nom), sep="\n"
                 )
               }
             },
             
             # 6 ─ Moy. observee vs theorique
             ttest1 = {
               n<-input$tt1_n; mu0<-input$tt1_mu; xbar<-input$tt1_xbar
               s<-input$tt1_s; alpha<-input$tt1_alpha; alt<-input$tt1_alt
               if(is.na(n)||n<1) stop("n doit etre un entier >= 1.")
               if(is.na(s)||s<=0) stop("s doit etre strictement positif.")
               vobs <- (xbar-mu0)/(s/sqrt(n))
               loi_lbl <- if(n>=30) "Normale N(0,1)" else sprintf("Student t (ddl=%d)",n-1)
               znr <- if(n>=30) znr_norm(alpha,alt) else znr_stud(alpha,alt,n-1)
               ap  <- znr$fn(vobs)
               h0 <- sprintf("mu = mu0 = %.4f",mu0)
               h1 <- if(alt=="bilateral")"mu != mu0" else if(alt=="droite")"mu > mu0" else "mu < mu0"
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  n=%d   x̄=%s   s=%s   mu0=%s   alpha=%.2f",n,V(xbar),V(s),V(mu0),alpha),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>%s</span>  (n %s 30)",loi_lbl,if(n>=30)">=" else "<"),
                 CL(n>=1,"n >= 1"), CL(s>0,"s > 0"),
                 CL(n>=30,"n >= 30 (ou normalite de la population supposee)"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  SE = s / sqrt(n) = %s",V(s/sqrt(n))),
                 STAT("stat_obs = (x̄ - mu0) / SE", vobs),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr$str,"stat_obs",vobs,alpha),
                 sprintf("  Valeur critique = %s",V(znr$crit)),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(ap) sprintf("Pas de difference significative avec mu0=%.4f.",mu0) else sprintf("La moyenne observee est significativement differente de mu0=%.4f.",mu0)),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 7 ─ 2 Proportions
             prop2 = {
               k1<-input$pp2_k1; n1<-input$pp2_n1; k2<-input$pp2_k2; n2<-input$pp2_n2
               alpha<-input$pp2_alpha; alt<-input$pp2_alt
               if(k1>n1) stop(sprintf("k₁=%d > n₁=%d : impossible.",k1,n1))
               if(k2>n2) stop(sprintf("k₂=%d > n₂=%d : impossible.",k2,n2))
               ph1<-k1/n1; ph2<-k2/n2; phc<-(k1+k2)/(n1+n2)
               c1 <- n1*ph1>=5 && n1*(1-ph1)>=5; c2 <- n2*ph2>=5 && n2*(1-ph2)>=5
               if(!c1||!c2) stop("Condition n.p >= 5 et n.(1-p) >= 5 non satisfaite. Augmentez les effectifs.")
               se <- sqrt(phc*(1-phc)*(1/n1+1/n2)); Zobs <- (ph1-ph2)/se
               znr <- znr_norm(alpha,alt); ap <- znr$fn(Zobs)
               h0 <- "p1 = p2"; h1 <- if(alt=="two.sided")"p1 != p2" else if(alt=="greater")"p1 > p2" else "p1 < p2"
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  Groupe 1 : n₁=%d  k₁=%d  p̂₁=%s",n1,k1,V(ph1)),
                 sprintf("  Groupe 2 : n₂=%d  k₂=%d  p̂₂=%s",n2,k2,V(ph2)),
                 sprintf("  p̂_c (proportion commune) = %s   alpha=%.2f",V(phc),alpha),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 "  Loi : <span class='r-value'>Normale N(0,1)  (approximation)</span>",
                 CL(c1,sprintf("n₁.p̂₁=%.1f >= 5  et  n₁.(1-p̂₁)=%.1f >= 5",n1*ph1,n1*(1-ph1))),
                 CL(c2,sprintf("n₂.p̂₂=%.1f >= 5  et  n₂.(1-p̂₂)=%.1f >= 5",n2*ph2,n2*(1-ph2))),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  SE = sqrt(p̂_c(1-p̂_c)(1/n₁+1/n₂)) = %s",V(se)),
                 STAT("Z_obs = (p̂1 - p̂2) / SE", Zobs),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr$str,"Z_obs",Zobs,alpha),
                 sprintf("  z_crit = %s",V(znr$crit)),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(ap)"Pas de difference significative entre les deux proportions." else "Les deux proportions sont significativement differentes."),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 8 ─ Proportion vs p0
             prop1 = {
               k<-input$p1_k; n<-input$p1_n; p0<-input$p1_p0; alpha<-input$p1_alpha; alt<-input$p1_alt
               if(k>n) stop(sprintf("k=%d > n=%d : impossible.",k,n))
               phat<-k/n; c1 <- n*phat>=5; c2 <- n*(1-phat)>=5
               if(!c1||!c2) stop(sprintf("Condition n.p̂>=5 non satisfaite (n.p̂=%.1f, n.(1-p̂)=%.1f).",n*phat,n*(1-phat)))
               se<-sqrt(p0*(1-p0)/n); Zobs<-(phat-p0)/se
               znr<-znr_norm(alpha,alt); ap<-znr$fn(Zobs)
               h0<-sprintf("p = p0 = %.4f",p0); h1<-if(alt=="two.sided")"p != p0" else if(alt=="greater")"p > p0" else "p < p0"
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  n=%d  k=%d  p̂=%s  p0=%s  alpha=%.2f",n,k,V(phat),V(p0),alpha),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 "  Loi : <span class='r-value'>Normale N(0,1)  (approximation)</span>",
                 CL(c1,sprintf("n.p̂ = %.1f >= 5",n*phat)),
                 CL(c2,sprintf("n.(1-p̂) = %.1f >= 5",n*(1-phat))),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  SE = sqrt(p0(1-p0)/n) = %s",V(se)),
                 STAT("Z_obs = (p̂ - p0) / SE", Zobs),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr$str,"Z_obs",Zobs,alpha),
                 sprintf("  z_crit = %s",V(znr$crit)),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(ap)sprintf("Pas de difference significative avec p0=%.4f.",p0) else sprintf("La proportion observee est significativement differente de p0=%.4f.",p0)),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 9 ─ k Proportions Chi-deux
             chisq_kprop = {
               k    <- max(2L,min(8L,as.integer(input$ckp_k%||%3)))
               alpha<-input$ckp_alpha; mode<-input$ckp_mode
               obs  <- sapply(seq_len(k), function(i){
                 v<-input[[paste0("ckp_o",i)]]
                 if(is.null(v)||is.na(v)||v<0) stop(sprintf("Effectif du groupe %d doit etre >= 0.",i))
                 as.integer(v)
               })
               n_tot<-sum(obs); if(n_tot==0) stop("La somme des effectifs est nulle.")
               tp <- if(mode=="equal") rep(1/k,k) else {
                 p<-pv(input$ckp_probs)
                 if(length(p)!=k) stop(sprintf("%d groupes mais %d probabilites.",k,length(p)))
                 if(abs(sum(p)-1)>1e-6) stop(sprintf("Les probabilites somment a %.4f != 1.",sum(p)))
                 p
               }
               theo<-n_tot*tp; cond_ei <- all(theo>=5)
               if(!cond_ei) stop(sprintf("Effectifs theoriques < 5 (min=%.2f). Augmentez n ou regroupez.",min(theo)))
               chi2<-sum((obs-theo)^2/theo); ddl<-k-1; Xc<-qchisq(1-alpha,df=ddl)
               ap <- (chi2 <= Xc)
               tl <- paste(sapply(seq_len(k), function(i)
                 sprintf("  G%d: Oi=%d  Ei=%s  pi=%s  contrib=%s",
                         i,obs[i],V(theo[i]),V(tp[i]),V((obs[i]-theo[i])^2/theo[i]))), collapse="\n")
               paste(
                 S(1,"DONNEES SAISIES"), tl,
                 sprintf("  k=%d  n=%d  alpha=%.2f  Loi H0 : %s",k,n_tot,alpha,if(mode=="equal")"equirepartition" else "personnalisee"),
                 S(2,"FORMULATION DES HYPOTHESES"),
                 HYP("La distribution suit la loi theorique","Au moins une proportion differe de la loi theorique"),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Chi-deux chi2(%d)</span>",ddl),
                 CL(cond_ei,sprintf("Tous les Ei >= 5  (Ei min = %.2f)",min(theo))),
                 CL(n_tot>=30,"n total >= 30"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 "  chi2_obs = Sum[(Oi - Ei)² / Ei]",
                 STAT("chi2_obs", chi2),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  ZNR = [ 0 ; %s ]   (chi2(%d), alpha=%.2f)",V(Xc),ddl,alpha),
                 sprintf("  chi2_crit = %s",V(Xc)),
                 STAT("chi2_obs", chi2),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(!ap)"La distribution observee differe significativement de la loi theorique." else "La distribution observee est conforme a la loi theorique."),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 10 ─ Fisher 2 variances
             fisher_var = {
               n1<-input$fv_n1; s1<-input$fv_s1; n2<-input$fv_n2; s2<-input$fv_s2
               alpha<-input$fv_alpha; type<-input$fv_type
               if(is.na(s1)||s1<=0) stop("s₁ doit etre strictement positif.")
               if(is.na(s2)||s2<=0) stop("s₂ doit etre strictement positif.")
               v1<-s1^2; v2<-s2^2
               h0<-"sigma1² = sigma2²"
               h1<-if(type=="bilateral")"sigma1² != sigma2²" else if(type=="droite")"sigma1² > sigma2²" else "sigma1² < sigma2²"
               if(type=="bilateral"){
                 Fobs<-max(v1,v2)/min(v1,v2)
                 dfn<-if(v1>=v2) n1-1 else n2-1; dfd<-if(v1>=v2) n2-1 else n1-1
                 Fc<-qf(1-alpha/2,dfn,dfd); ap<-(Fobs<Fc); znr_s<-sprintf("[ 0 ; %s ]",V(Fc))
               } else if(type=="droite"){
                 Fobs<-v1/v2; dfn<-n1-1; dfd<-n2-1; Fc<-qf(1-alpha,dfn,dfd); ap<-(Fobs<=Fc)
                 znr_s<-sprintf("[ 0 ; %s ]",V(Fc))
               } else {
                 Fobs<-v1/v2; dfn<-n1-1; dfd<-n2-1; Fc<-qf(alpha,dfn,dfd); ap<-(Fobs>=Fc)
                 znr_s<-sprintf("[ %s ; +inf [",V(Fc))
               }
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  Echantillon 1 : n₁=%d   s₁=%s   s₁²=%s",n1,V(s1),V(v1)),
                 sprintf("  Echantillon 2 : n₂=%d   s₂=%s   s₂²=%s",n2,V(s2),V(v2)),
                 sprintf("  alpha=%.2f   Test : %s",alpha,type),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Fisher F(%d, %d)</span>",dfn,dfd),
                 CL(n1>=2,"n₁ >= 2"), CL(n2>=2,"n₂ >= 2"),
                 CL(s1>0,"s₁ > 0"), CL(s2>0,"s₂ > 0"),
                 CL(TRUE,"Normalite des deux populations supposee"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  F = max(s²) / min(s²) = %.4f / %.4f",max(v1,v2),min(v1,v2)),
                 STAT("F_obs", Fobs),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  ZNR = %s   F_crit=%s   (F(%d,%d), alpha=%.2f)",znr_s,V(Fc),dfn,dfd,alpha),
                 STAT("F_obs", Fobs),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(!ap)"Les deux variances sont significativement differentes (heteroscedasticite)." else "Pas de difference significative de variance. Homoscedasticite maintenue."),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 11 ─ k Variances Levene / Bartlett / Brown-Forsythe
             var_k = {
               k    <- max(3L,min(8L,as.integer(input$vk_k%||%3)))
               alpha<-input$vk_alpha
               grps <- lapply(seq_len(k), function(i){
                 v<-pv(input[[paste0("vk_g",i)]])
                 if(length(v)<2) stop(sprintf("Groupe %d : au moins 2 valeurs.",i)); v
               })
               ni<-sapply(grps,length); N<-sum(ni)
               y<-unlist(grps); gf<-factor(rep(seq_len(k),ni))
               mi<-sapply(grps,mean)
               Zl<-lapply(seq_along(grps),function(i) abs(grps[[i]]-mi[i]))
               Zml<-sapply(Zl,mean); Z..<-sum(ni*Zml)/N
               W_lev<-((N-k)/(k-1))*sum(ni*(Zml-Z..)^2)/sum(sapply(seq_along(grps),function(i) sum((Zl[[i]]-Zml[i])^2)))
               medi<-sapply(grps,median)
               Zbf<-lapply(seq_along(grps),function(i) abs(grps[[i]]-medi[i]))
               Zmbf<-sapply(Zbf,mean); Z..bf<-sum(ni*Zmbf)/N
               W_bf<-((N-k)/(k-1))*sum(ni*(Zmbf-Z..bf)^2)/sum(sapply(seq_along(grps),function(i) sum((Zbf[[i]]-Zmbf[i])^2)))
               rb<-bartlett.test(y~gf); K2<-as.numeric(rb$statistic)
               Fc<-qf(1-alpha,k-1,N-k); Xcb<-qchisq(1-alpha,df=k-1)
               rej_lev <- (W_lev >= Fc)
               rej_bf  <- (W_bf  >= Fc)
               rej_bar <- (K2    >= Xcb)
               rl<-if(rej_lev)"<span class='r-ok'>REJET H0</span>" else "<span class='r-warn'>NON-REJET H0</span>"
               rb_<-if(rej_bf)"<span class='r-ok'>REJET H0</span>" else "<span class='r-warn'>NON-REJET H0</span>"
               rB<-if(rej_bar)"<span class='r-ok'>REJET H0</span>" else "<span class='r-warn'>NON-REJET H0</span>"
               n_rej<-sum(c(rej_lev, rej_bf, rej_bar))
               gl <- paste(sapply(seq_len(k), function(i)
                 sprintf("  G%d: n=%d  moy=%s  s=%s  var=%s",i,ni[i],V(mean(grps[[i]])),V(sd(grps[[i]])),V(var(grps[[i]])))), collapse="\n")
               paste(
                 S(1,"DONNEES SAISIES"), gl,
                 sprintf("  k=%d groupes   N=%d obs.   alpha=%.2f",k,N,alpha),
                 S(2,"FORMULATION DES HYPOTHESES"),
                 HYP("sigma1² = sigma2² = ... = sigmak²","Au moins une variance differe"),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 "  3 tests calcules simultanement :",
                 "  Levene       → Loi F(k-1, N-k)  — robuste si non-normalite",
                 "  Brown-Forsythe → Loi F(k-1, N-k)  — le plus robuste",
                 "  Bartlett     → Loi chi2(k-1)     — sensible a la non-normalite",
                 CL(k>=3,sprintf("k=%d >= 3",k)),
                 CL(all(ni>=2),"Chaque groupe a au moins 2 observations"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  Test Levene         : W = %s",V(W_lev)),
                 sprintf("  Test Brown-Forsythe : W = %s  (recommande)",V(W_bf)),
                 sprintf("  Test Bartlett       : K² = %s",V(K2)),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  F_crit(alpha=%.2f) = %s   chi2_crit = %s",alpha,V(Fc),V(Xcb)),
                 sprintf("  Levene        : W=%s  vs F_crit=%s  → %s",V(W_lev),V(Fc),rl),
                 sprintf("  Brown-Forsythe: W=%s  vs F_crit=%s  → %s",V(W_bf),V(Fc),rb_),
                 sprintf("  Bartlett      : K²=%s  vs chi2_crit=%s  → %s",V(K2),V(Xcb),rB),
                 S(6,"DECISION ET CONCLUSION"),
                 if(n_rej>=2)"<span class='r-ok'>  ✔  REJET H0  (majorite des tests)</span>" else if(n_rej==0)"<span class='r-warn'>  ✘  NON-REJET H0  (tous les tests concordent)</span>" else "<span class='r-warn'>  △  RESULTATS DIVERGENTS — interpretez avec prudence</span>",
                 INTERP(if(n_rej>=2)"Variances significativement differentes. L'ANOVA classique est deconseille." else if(n_rej==0)"Homoscedasticite. L'ANOVA classique est applicable." else "Resultats divergents. Preferer Brown-Forsythe (plus robuste)."),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 12 ─ ANOVA 2 facteurs sans replication
             anova2 = {
               nc   <- max(2L,min(6L,as.integer(input$an2_ncols%||%3)))
               alpha<-input$an2_alpha
               cols <- lapply(seq_len(nc), function(j){
                 v<-pv(input[[paste0("an2_c",j)]])
                 if(length(v)<2) stop(sprintf("Colonne B%d : au moins 2 valeurs.",j)); v
               })
               lens<-sapply(cols,length)
               if(length(unique(lens))!=1) stop(sprintf("Colonnes de longueurs differentes : %s",paste(lens,collapse=", ")))
               tab<-do.call(cbind,cols); nr<-nrow(tab); nc2<-ncol(tab)
               T_tot<-sum(tab); CF<-T_tot^2/(nr*nc2)
               TL<-rowSums(tab); TC<-colSums(tab)
               SSC<-sum(TC^2)/nr-CF; SSR<-sum(TL^2)/nc2-CF; SST<-sum(tab^2)-CF; SSE<-SST-SSC-SSR
               dc<-nc2-1; dr<-nr-1; de<-dr*dc
               MSC<-SSC/dc; MSR<-SSR/dr; MSE<-SSE/de; FC<-MSC/MSE; FR<-MSR/MSE
               Fcc<-qf(1-alpha,dc,de); Fcr<-qf(1-alpha,dr,de)
               ap_A <- (FR <= Fcr); ap_B <- (FC <= Fcc)
               dA<-if(!ap_A)"<span class='r-ok'>REJET H0 — Facteur A SIGNIFICATIF</span>" else "<span class='r-warn'>NON-REJET H0 — Facteur A non significatif</span>"
               dB<-if(!ap_B)"<span class='r-ok'>REJET H0 — Facteur B SIGNIFICATIF</span>" else "<span class='r-warn'>NON-REJET H0 — Facteur B non significatif</span>"
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  Tableau %d x %d   (lignes=niveaux A, colonnes=niveaux B)",nr,nc2),
                 sprintf("  Somme totale=%s   alpha=%.2f",V(T_tot),alpha),
                 S(2,"FORMULATION DES HYPOTHESES"),
                 HYP("Pas d'effet du facteur A (lignes)","Effet A significatif"),
                 HYP("Pas d'effet du facteur B (colonnes)","Effet B significatif"),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi facteur A : <span class='r-value'>F(%d, %d)</span>",dr,de),
                 sprintf("  Loi facteur B : <span class='r-value'>F(%d, %d)</span>",dc,de),
                 CL(nr>=2,"Au moins 2 niveaux pour A"), CL(nc2>=2,"Au moins 2 niveaux pour B"),
                 CL(TRUE,"Normalite et independance supposees"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  SST=%s  SSA=%s  SSB=%s  SSE=%s",V(SST),V(SSR),V(SSC),V(SSE)),
                 sprintf("  MSA=%s  MSB=%s  MSE=%s",V(MSR),V(MSC),V(MSE)),
                 STAT("F_obs (Facteur A)", FR),
                 STAT("F_obs (Facteur B)", FC),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  F_crit A = %s   (F(%d,%d), alpha=%.2f)",V(Fcr),dr,de,alpha),
                 sprintf("  F_crit B = %s   (F(%d,%d), alpha=%.2f)",V(Fcc),dc,de,alpha),
                 sprintf("  Facteur A : F_obs=%s  vs F_crit=%s",V(FR),V(Fcr)),
                 sprintf("  Facteur B : F_obs=%s  vs F_crit=%s",V(FC),V(Fcc)),
                 S(6,"DECISION ET CONCLUSION"),
                 sprintf("  Facteur A : %s",dA),
                 sprintf("  Facteur B : %s",dB),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 13 ─ Chi-deux d'independance
             chisq_indep = {
               r    <- max(2L,min(6L,as.integer(input$ci_nrows%||%2)))
               alpha<-input$ci_alpha
               rows <- lapply(seq_len(r), function(i){
                 v<-suppressWarnings(as.integer(pv(input[[paste0("ci_r",i)]])))
                 if(length(v)<2) stop(sprintf("Ligne %d : au moins 2 colonnes.",i)); v
               })
               lens<-sapply(rows,length)
               if(length(unique(lens))!=1) stop(sprintf("Lignes de longueurs differentes : %s",paste(lens,collapse=", ")))
               mat<-do.call(rbind,rows); nr_m<-nrow(mat); nc_m<-ncol(mat)
               yates<-as.logical(input$ci_yates)&&all(dim(mat)==c(2,2))
               res<-chisq.test(mat,correct=yates)
               chi2<-as.numeric(res$statistic); ddl<-as.numeric(res$parameter)
               Xc<-qchisq(1-alpha,df=ddl); n_tot<-sum(mat)
               ap <- (chi2 <= Xc)
               V_<-sqrt(chi2/(n_tot*(min(nr_m,nc_m)-1)))
               force_v <- min(res$expected)
               iv_<-if(V_<.1)"negligeable" else if(V_<.3)"faible" else if(V_<.5)"moderee" else "forte"
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  Tableau de contingence %d x %d   n=%d",nr_m,nc_m,n_tot),
                 if(yates)"  (Correction de Yates appliquee — tableau 2x2)" else "",
                 sprintf("  alpha=%.2f",alpha),
                 S(2,"FORMULATION DES HYPOTHESES"),
                 HYP("Les deux variables sont independantes","Il existe une liaison significative"),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Chi-deux chi2(%d)</span>  (ddl = (r-1)(c-1))",ddl),
                 CL(force_v>=5,sprintf("Effectifs theoriques min = %.2f >= 5",force_v)),
                 CL(n_tot>=20,"n total >= 20"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  chi2_obs = Sum[(Oij - Eij)² / Eij]"),
                 sprintf("  V de Cramer = sqrt(chi2 / (n.min(r,c)-1)) = %s   (%s)",V(V_),iv_),
                 STAT("chi2_obs", chi2),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  ZNR = [ 0 ; %s ]   (chi2(%d), alpha=%.2f)",V(Xc),ddl,alpha),
                 sprintf("  chi2_crit = %s",V(Xc)),
                 STAT("chi2_obs", chi2),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(!ap)sprintf("Les variables sont liees (V de Cramer=%.4f → association %s).",V_,iv_) else "Les deux variables sont independantes."),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 14 ─ Correlation de Pearson
             pearson = {
               x<-pv(input$cor_p_x); y<-pv(input$cor_p_y)
               alpha<-input$cor_p_alpha; alt<-input$cor_p_alt; r0<-input$cor_p_r0
               n<-length(x)
               if(length(x)!=length(y)) stop(sprintf("X a %d valeurs, Y en a %d.",length(x),length(y)))
               if(n<3) stop("Minimum 3 paires requises.")
               cov_xy<-sum((x-mean(x))*(y-mean(y)))/(n-1)
               et_x<-sd(x); et_y<-sd(y)
               rp<-cov_xy/(et_x*et_y)
               if(abs(rp)>=1-1e-9) stop("rp = +/-1 : correlation parfaite, test impossible.")
               tobs<-(rp-r0)*sqrt(n-2)/sqrt(1-rp^2); ddl<-n-2
               h0<-sprintf("rho = r0 = %.4f",r0); h1<-if(alt=="bilateral")"rho != r0" else if(alt=="droite")"rho > r0" else "rho < r0"
               znr<-znr_stud(alpha,alt,ddl); ap<-znr$fn(tobs)
               ir<-if(abs(rp)<.3)"faible" else if(abs(rp)<.7)"moderee" else "forte"
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  X = %s",paste(round(x,3),collapse=", ")),
                 sprintf("  Y = %s",paste(round(y,3),collapse=", ")),
                 sprintf("  n=%d   r0=%.4f   alpha=%.2f",n,r0,alpha),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Student t (ddl=%d)</span>",ddl),
                 CL(n>=3,sprintf("n=%d >= 3 paires",n)),
                 CL(TRUE,"Normalite bivariee supposee"),
                 CL(TRUE,"Liaison supposee lineaire"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  cov(X,Y) = %s   s_X = %s   s_Y = %s",V(cov_xy),V(et_x),V(et_y)),
                 sprintf("  rp = cov(X,Y) / (s_X . s_Y) = %s  (correlation %s)",V(rp),ir),
                 STAT("t_obs = (rp - r0).sqrt(n-2) / sqrt(1-rp²)", tobs),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr$str,"t_obs",tobs,alpha),
                 sprintf("  t_crit = %s",V(znr$crit)),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                 INTERP(if(!ap&&rp>0)sprintf("Correlation positive significative (rp=%.4f — %s). X croit => Y croit.",rp,ir) else if(!ap&&rp<0)sprintf("Correlation negative significative (rp=%.4f — %s). X croit => Y decroit.",rp,ir) else sprintf("Pas de correlation lineaire significative (rp=%.4f — %s).",rp,ir)),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 15 ─ Correlation de Spearman
             spearman = {
               x<-pv(input$cor_s_x); y<-pv(input$cor_s_y)
               alpha<-input$cor_s_alpha; alt<-input$cor_s_alt
               if(length(x)!=length(y)) stop(sprintf("X (%d) et Y (%d) longueurs differentes.",length(x),length(y)))
               if(length(x)<3) stop("Minimum 3 paires.")
               n<-length(x)
               rs <- cor(x, y, method="spearman")
               ts <- rs * sqrt((n-2) / (1-rs^2))
               ddl_s <- n-2
               znr_s <- znr_stud(alpha, alt, ddl_s); ap_s <- znr_s$fn(ts)
               ir<-if(abs(rs)<.3)"faible" else if(abs(rs)<.7)"moderee" else "forte"
               h0<-"rho_s = 0  (pas d'association monotone)"
               h1<-sprintf("rho_s %s 0",if(alt=="two.sided")"!=" else if(alt=="greater")">" else "<")
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  X = %s",paste(round(x,3),collapse=", ")),
                 sprintf("  Y = %s",paste(round(y,3),collapse=", ")),
                 sprintf("  n=%d   alpha=%.2f",n,alpha),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Student t (ddl=%d)</span>  (via rangs)",ddl_s),
                 CL(n>=3,sprintf("n=%d >= 3 paires",n)),
                 CL(TRUE,"Variables ordinales ou continues"),
                 CL(TRUE,"Pas de condition de normalite"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  rho_s = %s  (correlation de Spearman sur les rangs — %s)",V(rs),ir),
                 sprintf("  rho_s² = %s",V(rs^2)),
                 STAT("t_obs = rho_s . sqrt((n-2)/(1-rho_s²))", ts),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr_s$str,"t_obs",ts,alpha),
                 sprintf("  t_crit = %s",V(znr_s$crit)),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap_s),
                 INTERP(if(!ap_s&&rs>0)sprintf("Association monotone croissante significative (rho_s=%.4f — %s).",rs,ir) else if(!ap_s&&rs<0)sprintf("Association monotone decroissante significative (rho_s=%.4f — %s).",rs,ir) else sprintf("Pas d'association monotone significative (rho_s=%.4f — %s).",rs,ir)),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 16 ─ Correlation de Kendall
             kendall = {
               x<-pv(input$cor_k_x); y<-pv(input$cor_k_y)
               alpha<-input$cor_k_alpha; alt<-input$cor_k_alt
               if(length(x)!=length(y)) stop(sprintf("X (%d) et Y (%d) longueurs differentes.",length(x),length(y)))
               if(length(x)<3) stop("Minimum 3 paires.")
               n<-length(x)
               tau <- cor(x, y, method="kendall")
               # Approximation normale pour tau de Kendall
               sigma_tau <- sqrt((2*(2*n+5)) / (9*n*(n-1)))
               zk <- tau / sigma_tau
               znr_k <- znr_norm(alpha, alt); ap_k <- znr_k$fn(zk)
               it<-if(abs(tau)<.2)"faible" else if(abs(tau)<.5)"moderee" else "forte"
               h0<-"tau = 0  (pas de concordance)"; h1<-sprintf("tau %s 0",if(alt=="two.sided")"!=" else if(alt=="greater")">" else "<")
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  X = %s",paste(round(x,3),collapse=", ")),
                 sprintf("  Y = %s",paste(round(y,3),collapse=", ")),
                 sprintf("  n=%d   alpha=%.2f",n,alpha),
                 S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 "  Loi : <span class='r-value'>Normale N(0,1)  (approximation pour n >= 10)</span>",
                 CL(n>=3,sprintf("n=%d >= 3 paires",n)),
                 CL(TRUE,"Variables ordinales ou continues"),
                 CL(TRUE,"Pas de condition de normalite"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  tau_K = %s  (concordance %s)",V(tau),it),
                 sprintf("  sigma_tau = sqrt[2(2n+5)/(9n(n-1))] = %s",V(sigma_tau)),
                 STAT("Z_obs = tau / sigma_tau", zk),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 ZNR_LINE(znr_k$str,"Z_obs",zk,alpha),
                 sprintf("  z_crit = %s",V(znr_k$crit)),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap_k),
                 INTERP(if(!ap_k&&tau>0)sprintf("Concordance positive significative (tau=%.4f — %s).",tau,it) else if(!ap_k&&tau<0)sprintf("Concordance negative significative (tau=%.4f — %s) : tendance inverse.",tau,it) else sprintf("Pas de concordance significative (tau=%.4f — %s).",tau,it)),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 17 ─ Regression lineaire simple
             reg_lin = {
               X<-pv(input$rl_x); Y<-pv(input$rl_y); n<-length(X)
               if(length(X)!=length(Y)) stop(sprintf("X (%d) et Y (%d) longueurs differentes.",length(X),length(Y)))
               if(n<3) stop("Minimum 3 paires.")
               if(var(X)==0) stop("X est constante : impossible d'ajuster une droite.")
               xb<-mean(X); yb<-mean(Y)
               r<-sum((X-xb)*(Y-yb))/sqrt(sum((X-xb)^2)*sum((Y-yb)^2))
               sx<-sd(X); sy<-sd(Y); b<-r*(sy/sx); a<-yb-b*xb
               Yp<-a+b*X; SSres<-sum((Y-Yp)^2); SStot<-sum((Y-yb)^2); R2<-1-SSres/SStot
               MSE_<-SSres/(n-2); se_b<-sqrt(MSE_/sum((X-xb)^2)); t_b<-b/se_b; ddl_b<-n-2
               znr_b<-znr_stud(alpha=.05,"bilateral",ddl_b); ap_b<-znr_b$fn(t_b)
               ir<-if(abs(r)<.3)"faible" else if(abs(r)<.7)"moderee" else "forte"
               paste(
                 S(1,"DONNEES SAISIES"),
                 sprintf("  X = %s",paste(round(X,3),collapse=", ")),
                 sprintf("  Y = %s",paste(round(Y,3),collapse=", ")),
                 sprintf("  n=%d",n),
                 S(2,"FORMULATION DES HYPOTHESES"),
                 HYP("b = 0  (X n'explique pas Y, pas de liaison lineaire)",
                     "b != 0  (liaison lineaire significative)"),
                 S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                 sprintf("  Loi : <span class='r-value'>Student t(%d)</span>  (test sur la pente b)",ddl_b),
                 CL(n>=3,sprintf("n=%d >= 3 paires",n)),
                 CL(var(X)>0,"Var(X) > 0  (X non constante)"),
                 CL(TRUE,"Linearite de la relation supposee"),
                 CL(TRUE,"Residus normaux et homoscedastiques supposes"),
                 S(4,"VALEUR OBSERVEE DU TEST"),
                 sprintf("  x̄=%s   ȳ=%s   s_X=%s   s_Y=%s",V(xb),V(yb),V(sx),V(sy)),
                 sprintf("  r  (correlation)     = %s  (%s)",V(r),ir),
                 sprintf("  b  (pente)           = %s",V(b)),
                 sprintf("  a  (ordonnee orig.)  = %s",V(a)),
                 sprintf("  R² (determination)   = %s  (%.1f%% de variance expliquee)",V(R2),100*R2),
                 sprintf("  Droite estimee : y^ = %s + %s . X",V(a),V(b)),
                 STAT("t_obs (test sur b)", t_b),
                 S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                 sprintf("  ZNR = %s   (t(%d), alpha=0.05)",znr_b$str,ddl_b),
                 sprintf("  t_crit = %s",V(znr_b$crit)),
                 STAT("t_obs", t_b),
                 S(6,"DECISION ET CONCLUSION"), DEC_Z(ap_b),
                 INTERP(if(!ap_b&&b>0)sprintf("Pente b=%.4f significativement positive. Y augmente avec X. R²=%.4f.",b,R2) else if(!ap_b&&b<0)sprintf("Pente b=%.4f significativement negative. Y diminue avec X. R²=%.4f.",b,R2) else sprintf("Pente non significative (b=%.4f). Le modele lineaire n'est pas valide. R²=%.4f.",b,R2)),
                 "", FOOT(nom), sep="\n"
               )
             },
             
             # 18 ─ Test de Wilcoxon
             wilcox = {
               avant<-pv(input$wil_avant); apres<-pv(input$wil_apres)
               alpha<-input$wil_alpha; alt<-input$wil_alt
               if(length(avant)!=length(apres)) stop(sprintf("AVANT (%d) et APRES (%d) doivent avoir le meme nb de valeurs.",length(avant),length(apres)))
               if(length(avant)<3) stop("Minimum 3 paires.")
               diff_<-apres-avant; non_nulle<-diff_[diff_!=0]; n_eff<-length(non_nulle)
               if(n_eff==0) stop("Toutes les differences sont nulles — test impossible.")
               rangs<-rank(abs(non_nulle),ties.method="average")
               w_plus<-sum(rangs[non_nulle>0]); w_moins<-sum(rangs[non_nulle<0]); u_stat<-min(w_plus,w_moins)
               h0<-"med(avant) = med(apres)  (pas d'effet)"
               h1<-sprintf("med(avant) %s med(apres)",if(alt=="two.sided")"!=" else if(alt=="greater")">" else "<")
               if(n_eff<=30){
                 Tmax<-n_eff*(n_eff+1)/2; vp<-psignrank(0:Tmax,n_eff)
                 as_a<-if(alt=="two.sided") alpha/2 else alpha; iv_<-which(vp<=as_a)
                 if(length(iv_)==0){ znr_s<-"indefinie pour ce n et alpha"; ap<-TRUE } else {
                   bi<-max(iv_)-1
                   znr_s<-if(alt=="two.sided")sprintf("(%d ; %d)",bi+1,Tmax-bi-1) else sprintf("[%d ; +inf [",bi+1)
                   ap<-if(alt=="two.sided")(u_stat>bi&&u_stat<(Tmax-bi)) else (u_stat>bi)
                 }
                 paste(
                   S(1,"DONNEES SAISIES"),
                   sprintf("  AVANT : %s",paste(avant,collapse=", ")),
                   sprintf("  APRES : %s",paste(apres,collapse=", ")),
                   sprintf("  Differences di=APRES-AVANT : %s",paste(round(diff_,3),collapse=", ")),
                   sprintf("  Differences non nulles : %s",paste(round(non_nulle,3),collapse=", ")),
                   sprintf("  n_eff=%d   alpha=%.2f",n_eff,alpha),
                   S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                   S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                   "  Loi : <span class='r-value'>Table de Wilcoxon  (n_eff <= 30)</span>",
                   CL(n_eff>=1,"Au moins une difference non nulle"),
                   CL(length(avant)>=3,"Au moins 3 paires"),
                   CL(TRUE,"Distribution des differences supposee symetrique"),
                   S(4,"VALEUR OBSERVEE DU TEST"),
                   sprintf("  Rangs |di| : %s",paste(round(rangs,2),collapse=", ")),
                   sprintf("  W+ = %s   W- = %s",V(w_plus),V(w_moins)),
                   STAT("u = min(W+, W-)", u_stat),
                   S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                   sprintf("  ZNR (Wilcoxon n_eff=%d) = %s   (alpha=%.2f)",n_eff,znr_s,alpha),
                   STAT("u", u_stat),
                   S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                   INTERP(if(!ap)"Difference significative avant/apres (effet du traitement)." else "Pas de difference significative avant/apres."),
                   "", FOOT(nom), sep="\n"
                 )
               } else {
                 mu_w<-n_eff*(n_eff+1)/4; sg_w<-sqrt(n_eff*(n_eff+1)*(2*n_eff+1)/24)
                 Zobs<-(u_stat-mu_w)/sg_w
                 znr<-znr_norm(alpha,alt); ap<-znr$fn(Zobs)
                 paste(
                   S(1,"DONNEES SAISIES"),
                   sprintf("  n=%d paires   n_eff=%d   alpha=%.2f",length(avant),n_eff,alpha),
                   S(2,"FORMULATION DES HYPOTHESES"), HYP(h0,h1),
                   S(3,"LOI DE PROBABILITE ET CONDITIONS DE VALIDITE"),
                   "  Loi : <span class='r-value'>Normale N(0,1)  (approximation n_eff > 30)</span>",
                   CL(n_eff>30,"n_eff > 30 : approximation normale valide"),
                   S(4,"VALEUR OBSERVEE DU TEST"),
                   sprintf("  W+=%s  W-=%s  u=%s",V(w_plus),V(w_moins),V(u_stat)),
                   sprintf("  mu_W=%s   sigma_W=%s",V(mu_w),V(sg_w)),
                   STAT("Z_obs = (u - mu_W) / sigma_W", Zobs),
                   S(5,"POINTS CRITIQUES ET ZONE DE NON-REJET"),
                   ZNR_LINE(znr$str,"Z_obs",Zobs,alpha),
                   sprintf("  z_crit = %s",V(znr$crit)),
                   S(6,"DECISION ET CONCLUSION"), DEC_Z(ap),
                   INTERP(if(!ap)"Difference significative avant/apres." else "Pas de difference significative."),
                   "", FOOT(nom), sep="\n"
                 )
               }
             },
             
             sprintf('<span class="r-warn">Test "%s" non implemente.</span>', tid)
      )
    }, error=function(e){
      sprintf('<span class="r-warn">Erreur de saisie\n\n%s\n\nVerifiez : virgules entre les valeurs, pas de lettres, effectifs positifs.</span>',
              conditionMessage(e))
    })
  })
}

shinyApp(ui=ui, server=server)