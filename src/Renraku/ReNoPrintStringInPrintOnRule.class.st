"
#printString is implemented by manipulating a stream.
Using it inside a #printOn: will imply a useless creation of stream and its pass to #printOn: one, instead of using directely the stream parameter to #printOn:
"
Class {
	#name : #ReNoPrintStringInPrintOnRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReNoPrintStringInPrintOnRule class >> checksMethod [

	^ true
]

{ #category : #'testing-interest' }
ReNoPrintStringInPrintOnRule class >> initialize [
    ReRuleManager cleanUp 
]

{ #category : #running }
ReNoPrintStringInPrintOnRule >> basicCheck: aMethod [ 
	^ (aMethod selector = #printOn: and: [ aMethod messages anySatisfy: [ :aMessageSymbol | aMessageSymbol = #printString ] ])

]

{ #category : #accessing }
ReNoPrintStringInPrintOnRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReNoPrintStringInPrintOnRule >> name [
	^ 'No PrintString In PrintOn' 
]

{ #category : #accessing }
ReNoPrintStringInPrintOnRule >> rationale [
	^ 'Use the stream argument to #printOn: instead of creating an extra stream(by using printString)'
]

{ #category : #accessing }
ReNoPrintStringInPrintOnRule >> severity [
	^ #warning
]
