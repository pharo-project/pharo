menu actions
createWindowGroup

  | group pos ext |
  
  pos := self position.
  ext := self extent.
  group := GroupWindowMorph new.
  group addWindow: self.
  (group openInWindowLabeled: 'Group: ' translated, self label) 
    extent: ext;
    position: pos.
  