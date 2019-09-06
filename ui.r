shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "MLT"),
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenuOutput("EARL_sidebar")
  ),
  shinydashboard::dashboardBody(
    uiOutput("EARL_body"),
    tags$script("var el2 = document.querySelector('.skin-blue'); el2.className = 'skin-blue sidebar-mini'; $('body').addClass('sidebar-collapse');")
  )
)

