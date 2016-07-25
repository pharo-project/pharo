testing
testErrorClassCreation
    | tmpCategory trait aSubclass aClass |
	
    tmpCategory := 'TemporaryGeneratedClasses'.
    trait := Trait named: #TMyTrait uses: {} category: tmpCategory.
    
    [ aClass := nil
        subclass: #AClass
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: tmpCategory.    
	"----------------"
    aSubclass := aClass
        subclass: #AClass2
        uses: trait
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: tmpCategory.    
	"----------------"
    "Change the superclass of AClass"
    Object
        subclass: #AClass
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: tmpCategory.
        
    self assert: trait traitUsers asArray equals: {aSubclass}.
    self assert: aSubclass traits asArray equals: {trait}. 

    ] ensure: [
        #(AClass AClass2 TMyTrait) do: [ :aClassName |
            Smalltalk globals at: aClassName ifPresent: [ :v | v removeFromSystem ]]]