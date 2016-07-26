running
tearDown
  | aGofer finalWorkingCopyList diff |
  super tearDown.
  aGofer := Gofer new.
  self tearDownPackageNames
    do: [ :pName | 
      (self hasPackage: pName)
        ifTrue: [ aGofer package: pName ] ].
  aGofer references notEmpty
    ifTrue: [ aGofer metacelloUnload ].
  MCRepositoryGroup default
    removeIdenticalRepository:
        (Smalltalk at: #'Metacello_Gofer_Test_Repository' ifAbsent: [  ]);
    removeIdenticalRepository:
        (Smalltalk at: #'Metacello_Configuration_Test_Repository' ifAbsent: [  ]);
    yourself.
  Smalltalk removeKey: #'Metacello_Gofer_Test_Repository' ifAbsent: [  ].
  Smalltalk removeKey: #'Metacello_Configuration_Test_Repository' ifAbsent: [  ].
  finalWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ].
  diff := finalWorkingCopyList difference: initialWorkingCopyList.
  diff
    do: [ :leak | 
      Transcript
        cr;
        show:
            'leaked package from ' , self printString , ' -> ' , leak printString ].
  self assert: diff isEmpty