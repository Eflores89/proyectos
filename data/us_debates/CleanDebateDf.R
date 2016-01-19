# Clean debates for new data.frame

# new column 
debate_v$debate <- "Vegas"
debate_h$debate <- "Ohio"
debate_b$debate <- "Boulder"
debate_c$debate <- "California"
debate_s$debate <- "SouthCarolina"
debate_w$debate <- "Wisconsin"

all_debates <- rbind(debate_v, debate_h, debate_w, 
                     debate_b, debate_s, debate_c)

