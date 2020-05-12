p1 <- gf_dist("norm", mean=0, geom = "area", fill= ~(x>qnorm(0.95)), alpha = 0.5, 
        title=expression(paste("Fehler 1. Art: Verteilung bei ", mu==0)),
        subtitle=expression(H[0]:mu<=0 )) %>%
  gf_refine(guides(fill=guide_legend(title=expression(paste(alpha,"-Fehler"))))) %>%
  gf_refine(annotate("text",x=2.5, y=0.3, label="Kritischer Wert")) %>%
  gf_labs(x=expression(bar(x))) %>%
  gf_vline(xintercept = qnorm(0.95)) %>%
  gf_dist("norm", mean=2, alpha = 0.5) %>%
  gf_theme(theme = theme_bw())    
  


p2 <- gf_dist("norm", mean=2, geom = "area", fill= ~(x<qnorm(0.95)), alpha = 0.5, 
        title=expression(paste("Fehler 2. Art: Verteilung bei ", mu==2)),
        subtitle=expression(H[0]:mu<=0 )) %>%
  gf_refine(guides(fill=guide_legend(title=expression(paste(beta,"-Fehler"))))) %>%
  gf_refine(annotate("text",x=0.5, y=0.3, label="Kritischer Wert")) %>%
  gf_labs(x=expression(bar(x))) %>%
  gf_vline(xintercept = qnorm(0.95)) %>%
  gf_dist("norm", mean=0, alpha = 0.5) %>%
  gf_theme(theme = theme_bw())    

gridExtra::grid.arrange(p1,p2, nrow=2)