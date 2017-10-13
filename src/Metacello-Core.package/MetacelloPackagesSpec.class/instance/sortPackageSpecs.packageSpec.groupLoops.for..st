private
sortPackageSpecs: orderedSpecs packageSpec: packageSpec groupLoops: groupLoops for: aVersionSpec
  | packageIndex moved movePackage targetPackage targetIndex targetSpecNames groupLoopDetected |
  packageIndex := orderedSpecs indexOf: packageSpec.
  moved := movePackage := false.
  targetSpecNames := packageSpec
    requiredSpecNamesForPackageOrdering: aVersionSpec.
  groupLoopDetected := groupLoops includes: packageSpec.
  groupLoopDetected
    ifFalse: [ 
      targetSpecNames
        do: [ :targetPackageOrProjectName | 
          targetPackage := orderedSpecs
            detect: [ :each | each name = targetPackageOrProjectName ]
            ifNone: [ 
              MetacelloNameNotDefinedError
                signal:
                  'project group, or package named: ' , targetPackageOrProjectName printString
                    ,
                      ' not found when used in requires: or includes: field of package: '
                    , packageSpec name printString , ' for version: '
                    , aVersionSpec versionString , ' of '
                    , aVersionSpec projectLabel , '.'.
              nil	"return nil if resumed" ].
          targetIndex := orderedSpecs indexOf: targetPackage.
          (groupLoopDetected not and: [ packageIndex = targetIndex ])
            ifTrue: [ 
              self
                notify:
                  'A group loop has been detected. The package: ' , packageSpec name printString
                    ,
                      ' requires a group that includes itself. If you resume, the group loop will be ignored and details will be written to the Transcript.'.
              groupLoopDetected := true.
              groupLoops add: packageSpec.
              Transcript
                cr;
                show: 'Package: ' , packageSpec name printString;
                cr;
                tab;
                show: 'Raw Requires:'.
              packageSpec requires
                do: [ :each | 
                  Transcript
                    cr;
                    tab;
                    tab;
                    show: each ].
              Transcript
                cr;
                show:
                    'Package: ' , packageSpec name printString , ' has a group loop:';
                cr;
                tab;
                show: 'Expanded Requires:'.
              targetSpecNames
                do: [ :each | 
                  Transcript
                    cr;
                    tab;
                    tab;
                    show: each ] ].
          movePackage := movePackage or: [ packageIndex <= targetIndex ].
          false
            ifTrue: [ 
              "use for debugging non-obvious reference loops"
              packageIndex < targetIndex
                ifTrue: [ 
                  Transcript
                    cr;
                    show:
                        'Moving ' , targetPackageOrProjectName , ' from ' , targetIndex printString
                            , ' to ' , packageIndex printString ] ] ] ].
  groupLoopDetected
    ifTrue: [ 
      "old implementation that does not resolve group names"
      targetSpecNames := packageSpec requires.
      targetSpecNames
        do: [ :targetPackageName | 
          targetPackage := orderedSpecs
            detect: [ :each | each name = targetPackageName ]
            ifNone: [  ].
          targetIndex := orderedSpecs indexOf: targetPackage.
          movePackage := movePackage or: [ packageIndex <= targetIndex ] ] ].
  movePackage
    ifTrue: [ 
      moved := true.
      orderedSpecs
        remove: packageSpec
        ifAbsent: [ ^ self error: 'unexpected error removing package' ].
      targetIndex := 0.
      targetSpecNames
        do: [ :targetPackageOrProjectName | 
          (targetPackage := orderedSpecs
            detect: [ :each | each name = targetPackageOrProjectName ]
            ifNone: [  ]) ~~ nil
            ifTrue: [ targetIndex := targetIndex max: (orderedSpecs indexOf: targetPackage) ] ].
      targetIndex == 0
        ifTrue: [ orderedSpecs add: packageSpec beforeIndex: packageIndex ]
        ifFalse: [ orderedSpecs add: packageSpec afterIndex: targetIndex ] ].
  ^ moved