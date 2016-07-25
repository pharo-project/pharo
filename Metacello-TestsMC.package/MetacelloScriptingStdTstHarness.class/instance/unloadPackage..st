running
unloadPackage: packageName
  | aGofer |
  aGofer := Gofer new.
  (self hasPackage: packageName)
    ifTrue: [ aGofer package: packageName ].
  aGofer references notEmpty
    ifTrue: [ aGofer unload ]