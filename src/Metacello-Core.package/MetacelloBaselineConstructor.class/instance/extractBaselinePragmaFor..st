pragma extraction
extractBaselinePragmaFor: aClass
    | pragmas |
    pragmas := Pragma allNamed: #'baseline' in: aClass.
    pragmas isEmpty
        ifTrue: [ ^ self error: 'No #baseline pragma found' ].
    ^ pragmas first