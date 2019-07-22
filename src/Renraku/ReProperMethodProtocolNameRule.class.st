"
This is an abstract rule class to check for an appropriate method
protol name.

Subclasses should override #protocolIdiom to return an association of a good protocol name and the bad protocol names it should replace.
"
Class {
	#name : #ReProperMethodProtocolNameRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReProperMethodProtocolNameRule class >> checksMethod [
	"This one is abstract but subclasses should run as rules"
	
	^ self name ~= #ReProperMethodProtocolNameRule
]

{ #category : #'private - utilities' }
ReProperMethodProtocolNameRule class >> use: valid insteadOf: arrayOfInvalid [
	"Instead of directly creating an array we use this utility method for better readability
	 in senders of this method"
	
	^valid -> arrayOfInvalid 
]

{ #category : #'private - accessing' }
ReProperMethodProtocolNameRule >> badMethodProtocolNames [

	^self protocolIdiom value collect: #asSymbol
]

{ #category : #running }
ReProperMethodProtocolNameRule >> basicCheck: aMethod [
	^ self badMethodProtocolNames includes: aMethod category
]

{ #category : #'private - accessing' }
ReProperMethodProtocolNameRule >> goodMethodProtocolName [

	^self protocolIdiom key printString
]

{ #category : #accessing }
ReProperMethodProtocolNameRule >> group [

	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
ReProperMethodProtocolNameRule >> name [
	
	^ 'Method categorization: use ', self goodMethodProtocolName, ' as protocol name' 
]

{ #category : #idioms }
ReProperMethodProtocolNameRule >> protocolIdiom [
	"Subclasses should override to return an association between a
	 wished category and an array of category names that are not so good."
 
	^self subclassResponsibility
]

{ #category : #accessing }
ReProperMethodProtocolNameRule >> rationale [
	^ 'Check if the method protocol name is appropriate and fulfils common expectations.'
]

{ #category : #accessing }
ReProperMethodProtocolNameRule >> severity [

	^ #information
]

{ #category : #'private - utilities' }
ReProperMethodProtocolNameRule >> use: valid insteadOf: arrayOfInvalid [
	"Instead of directly creating an array we use this utility method for better readability
	 in senders of this method"
	
	^self class use: valid insteadOf: arrayOfInvalid
]
