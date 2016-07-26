loading
ensureLoadUsing: aLoader
  | pp |
  pp := (MetacelloLookupProjectSpec new
    projectSpec: self;
    yourself) signal projectPackage.
  pp
    ifNil: [ 
      self
        error:
          'Unable to resolve project package for ' , self name printString
            ,
              '. It is likely that that the configuration referencing this project will not validate properly (see MetacelloToolBox class>>validateConfiguration:).' ].
  pp ensureLoadUsing: aLoader