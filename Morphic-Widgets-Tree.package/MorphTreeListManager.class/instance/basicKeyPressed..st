keyboard managing
basicKeyPressed: aChar
    | nextSelection oldSelection milliSeconds slowKeyStroke nextSelectionNodeMorph |
   
    (aChar == Character space and: [ lastClickedMorph notNil ])
        ifTrue: [
            self
                selectMorph: lastClickedMorph
                multiple: (self isMultiple or: [self autoMultiSelection]).
            self selectionUpdateFromViewWhile: [ | selHolder |
                selHolder := self newSelectionHolderWithNodePath: lastClickedMorph complexContents path.
                self listModel selection: selHolder]].
       
    nextSelection := oldSelection := lastSelection.
    milliSeconds := Time millisecondClockValue.
    slowKeyStroke := milliSeconds - lastKeystrokeTime > 500.
    lastKeystrokeTime := milliSeconds.
   
    self searchedElement: nil.
   
    slowKeyStroke
        ifTrue: ["forget previous keystrokes and search in following elements"
            oldSelection := oldSelection + 1.
            lastKeystrokes := aChar asLowercase asString.]
        ifFalse: ["append quick keystrokes but don't move selection if it still matches"
            lastKeystrokes := lastKeystrokes , aChar asLowercase asString.].

    oldSelection isZero
        ifFalse:
            [ nextSelectionNodeMorph := self allNodeMorphs
                detect:
                    [ :a |
                    a index >= oldSelection and: [
                    "Get rid of blanks and style used in some lists"
                    a complexContents asString trimBoth asLowercase beginsWith: lastKeystrokes ]]
                ifNone: [     "roll over" oldSelection := 0 ] ].
               
    oldSelection isZero
        ifTrue:
            [ nextSelectionNodeMorph := self allNodeMorphs
                detect:
                    [ :a |
                    "Get rid of blanks and style used in some lists"
                    a complexContents asString trimBoth asLowercase beginsWith: lastKeystrokes ]
                ifNone: [ ^ self ] ].
           
    nextSelection := nextSelectionNodeMorph index.
    "No change if model is locked"
    self listModel okToChange ifFalse: [^ self].
    self searchedElement: nextSelectionNodeMorph.

    lastSelection := nextSelection.
    "change scrollbarvalue"
    self scrollToShow: nextSelectionNodeMorph.
    self selectionChanged