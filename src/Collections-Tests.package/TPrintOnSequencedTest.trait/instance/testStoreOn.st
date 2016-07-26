tests - printing
testStoreOn
" for the moment work only for collection that include simple elements such that Integer"

"| string str result cuttedResult index elementsAsStringExpected elementsAsStringObtained tmp |
string := ''.
str := ReadWriteStream  on: string.
elementsAsStringExpected := OrderedCollection new.
elementsAsStringObtained := OrderedCollection new.
self nonEmpty do: [ :each | elementsAsStringExpected  add: each asString].

self nonEmpty storeOn: str.
result := str contents .
cuttedResult := ( result findBetweenSubstrings: ';' ).

index := 1.

cuttedResult do:
	[ :each |
	index = 1 
		ifTrue: [ self assert: (each beginsWith: ( tmp := '((' , self nonEmpty class asString , ' new) add: '           )).
				tmp := each copyFrom: ( tmp size + 1) to: ( each size ).
				elementsAsStringObtained add: tmp.
				index := index + 1. ]
		ifFalse:  [ 
		 index < cuttedResult size
			ifTrue:[self assert: (each beginsWith: ( tmp:=  ' add: '   )).
				tmp := each copyFrom: ( tmp size + 1) to: ( each size ).
				elementsAsStringObtained add: tmp.
					index := index + 1.]
			ifFalse: [self assert: ( each = ' yourself)' ) ].
			]
	
	].


	elementsAsStringExpected do: [ :each | self assert: (elementsAsStringExpected occurrencesOf: each ) = ( elementsAsStringObtained occurrencesOf: each) ]"

