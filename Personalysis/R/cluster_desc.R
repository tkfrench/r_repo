col_n = 'r1'

# Generate html taylored to individual color category, e.g., 'r1' or 'y3'

html_str = switch(col_n,
  'r1' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r1r0r0b0b0b0g0g0g0y0y0y0.png)</td><td><B> Like to:</B><BR>Take immediate action <BR> Implement <BR> Focus on goals <BR> Execute</td></tr> </table>',
  'r2' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r2r0b0b0b0g0g0g0y0y0y0.png)</td><td><B> Should: </B><BR> Independent <BR> Assertive <BR> Direct <BR> Produce</td></tr> </table>',
  'r3' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r3b0b0b0g0g0g0y0y0y0.png)</td><td><B> Must have:</B><BR> Clear goal <BR> Personal experience <BR> Ability to initiate action <BR> WHAT? WHEN? </td></tr> </table>',
  'b1' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b1b0b0g0g0g0y0y0y0.png)</td><td><B> Like to </B> <BR>  Diagnose <BR> Coach/Consult <BR> Develop solutions <BR> Strategize</td></tr> </table>',
  'b2' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b2b0g0g0g0y0y0y0.png)</td><td><B> Should: </B><BR> Knowledgeable <BR> Understanding <BR> Question <BR> Explain</td></tr> </table> ',
  'b3' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b0b3g0g0g0y0y0y0.png)</td><td><B> Must have: </B> <BR> Rationale/Context <BR> Clarity <BR> Ability to contemplate <BR> WHY? WHAT IF?</td></tr> </table>',
  'g1' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b0b0g1g0g0y0y0y0.png)</td><td><B> Like to: </B><BR> Structure <BR> Monitor <BR> Prioritize work <BR> Control</td></tr> </table>',
  'g2' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b0b0g0g2g0y0y0y0.png)</td><td><B> Should: </B><BR> Responsible <BR> Consistent <BR> Comply <BR> Regulate</td></tr> </table>',
  'g3' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b0b0g0g0g3y0y0y0.png)</td><td><B> Must have: </B><BR> History of reliability <BR> Data and details <BR> Ability to prove <BR> HOW? WHERE?</td></tr> </table>',
  'y1' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b0b0g0g0g0y1y0y0.png)</td><td><B> Like to: </B><BR> Facilitate <BR> Include <BR> Actively support others <BR> Participate</td></tr> </table>',
  'y3' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b0b0g0g0g0y0y2y0.png)</td><td><B> Should: </B><BR>Responsive <BR> Diplomatic <BR> Involve <BR> Observe</td></tr> </table>',
  'y3' = '<table><tr><td  width = "235">![](/Users/tfrench/Desktop/R projects/Personalysis/images/r0r0r0b0b0b0g0g0g0y0y0y3.png)</td><td><B> Must have: </B><BR> Group support <BR> Flexibility <BR> Ability to influence others <BR> WHO? WHAT ELSE?</td></tr> </table>'
  )



 cat(paste0('\n  ', html_str,'\n' ) )

