*metacello-mc
resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool into: packages visited: visited
  visited
    visit: self
    doing: [ :aSpec | 
      | map |
      map := aVersionSpec packages map.
      aSpec includes
        do: [ :pkgName | 
          (aVersionSpec packageNamed: pkgName forMap: map ifAbsent: [  ])
            projectDo: [ :prj | 
              andProjectsBool
                ifTrue: [ packages at: prj name put: prj ] ]
            packageDo: [ :pkg | packages at: pkg name put: pkg ]
            groupDo: [ :grp | 
              grp
                resolveToPackagesIn: aVersionSpec
                andProjects: andProjectsBool
                into: packages
                visited: visited ] ] ]