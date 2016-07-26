accessing
packageSpecsInLoadOrderFor: aVersionSpec
  "order only the packageSpecs and projectSpecs. groupSpec order does not matter"

  | orderedPackageSpecs groupSpecs moved lastMovedSpecs count terminationLimit map specsWithIncludes firstTime groupLoops |
  map := self map.
  orderedPackageSpecs := OrderedCollection new.
  groupSpecs := Set new.
  self list
    do: [ :member | 
      | spec |
      spec := map at: member name ifAbsent: [  ].
      (spec == nil or: [ orderedPackageSpecs includes: spec ])
        ifFalse: [ 
          spec
            projectDo: [ :prjSpec | 
              "specification order is the default order"
              orderedPackageSpecs add: prjSpec ]
            packageDo: [ :pkgSpec | 
              "specification order is the default order"
              orderedPackageSpecs add: pkgSpec ]
            groupDo: [ :groupSpec | groupSpecs add: groupSpec ] ] ].
  orderedPackageSpecs isEmpty
    ifTrue: [ ^ orderedPackageSpecs , groupSpecs asOrderedCollection ].
  moved := true.
  count := 0.
  terminationLimit := orderedPackageSpecs size * 2.
  groupLoops := IdentitySet new.
  [ moved ]
    whileTrue: [ 
      count := count + 1.
      count > terminationLimit
        ifTrue: [ 
          "Cheap termination hack - an APPARENT loop"
          self error: 'Apparent loop in before/after dependency definitions' ].
      moved := false.
      orderedPackageSpecs copy
        do: [ :packageSpec | 
          moved := moved
            or: [ 
              self
                sortPackageSpecs: orderedPackageSpecs
                packageSpec: packageSpec
                groupLoops: groupLoops
                for: aVersionSpec ] ] ].
  lastMovedSpecs := Set new.
  moved := true.
  count := 0.
  specsWithIncludes := orderedPackageSpecs
    select: [ :pkgSpec | pkgSpec includesForPackageOrdering isEmpty not ].
  firstTime := true.
  [ moved ]
    whileTrue: [ 
      | result |
      count := count + 1.	"count > terminationLimit"
      count > 14
        ifTrue: [ 
          "Cheap termination hack - an APPARENT loop"
          self error: 'Apparent loop in before/after dependency definitions' ].
      moved := false.
      result := Set new.
      specsWithIncludes
        do: [ :packageSpec | 
          result
            addAll:
              (self
                applyIncludesTo: orderedPackageSpecs
                for: packageSpec
                firstTime: firstTime
                for: aVersionSpec) ].
      result size = lastMovedSpecs size
        ifTrue: [ 
          result
            do: [ :name | 
              (lastMovedSpecs includes: name)
                ifFalse: [ moved := true ] ] ]
        ifFalse: [ moved := true ].
      lastMovedSpecs := result.
      firstTime := false ].
  ^ orderedPackageSpecs , groupSpecs asOrderedCollection