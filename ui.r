dashboardPage(
  dashboardHeader(title = "MLT"),
  dashboardSidebar(sidebarMenuOutput("EARL_sidebar")
  ),
  dashboardBody(
    uiOutput("EARL_body"),
    tags$script("var el2 = document.querySelector('.skin-blue'); el2.className = 'skin-blue sidebar-mini'; $('body').addClass('sidebar-collapse');")
  )
)