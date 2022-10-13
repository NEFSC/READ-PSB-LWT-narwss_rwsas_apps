library(DiagrammeR)

grViz("
      
digraph boxes_and_circles {

node [shape = box, color = salmon,  penwidth = 2]
Acoustic
'Upcall(s) detected'
'Around position of 
acoustic platform'

node [shape = box, color = darkturquoise, penwidth = 2]
Visual
'Right whales seen'
'Around the core area of 
right whale sightings'
'Do the core areas meet the 
trigger density threshold?'

node [shape = box, color = black, penwidth = 2]
'Do detections fall within active protection zones?'  
'Is the zone set to expire 
within the next 7 days?'
'Calculate 15 nm 
SLOW zone boundaries'
'Request to extend currently active 
dynamic SLOW zone'

edge [color = salmon]
Acoustic->'Upcall(s) detected'->'Do detections fall within active protection zones?'
'Do detections fall within active protection zones?'->'Is the zone set to expire 
within the next 7 days?'[label = 'yes']
'Is the zone set to expire 
within the next 7 days?'->'Request to extend currently active 
dynamic SLOW zone'[label = 'yes'];
'Do detections fall within active protection zones?'->'Calculate 15 nm 
SLOW zone boundaries'[label = 'no'];
'Calculate 15 nm 
SLOW zone boundaries'->'Around position of 
acoustic platform'


edge [color = darkturquoise, arrowhead = diamond]

Visual->'Right whales seen'->'Do detections fall within active protection zones?'->  
'Is the zone set to expire 
within the next 7 days?'->'Request to extend currently active 
dynamic SLOW zone';
'Do detections fall within active protection zones?'->'Do the core areas meet the 
trigger density threshold?'[label = 'no']
'Do the core areas meet the 
trigger density threshold?'->'Calculate 15 nm 
SLOW zone boundaries'[label = 'yes']
'Calculate 15 nm 
SLOW zone boundaries'->'Around the core area of 
right whale sightings'

}
            ")

