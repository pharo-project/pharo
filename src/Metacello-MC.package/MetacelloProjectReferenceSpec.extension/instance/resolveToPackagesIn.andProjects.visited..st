*metacello-mc
resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool visited: visited
  ^ andProjectsBool
    ifTrue: [ {self} ]
    ifFalse: [ self resolveToPackagesIn: aVersionSpec visited: visited ]