dropping/grabbing
wantsDroppedMorph: aMorph event: evt
  "Accept if a SystemWindow."
  
  self visible ifFalse: [^ false].
  self dropEnabled ifFalse: [^ false].
  
  (self tabGroup tabSelectorMorph bounds containsPoint: evt position) ifFalse: [^ false].
  
  ^aMorph isSystemWindow