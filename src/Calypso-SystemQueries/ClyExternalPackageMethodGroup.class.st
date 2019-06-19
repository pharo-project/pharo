"
I am group of class extension methods from concrete package.
 
I represent subgroups of ""extensions"" method group.

Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<RPackage>
"
Class {
	#name : #ClyExternalPackageMethodGroup,
	#superclass : #ClyMethodGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClyExternalPackageMethodGroup class >> isEditableGroup [
	^true
]

{ #category : #'instance creation' }
ClyExternalPackageMethodGroup class >> withMethodsFrom: aClassScope packagedIn: aPackage [

	| methodQuery |
	methodQuery := ClyPackageExtensionMethodsQuery of: aPackage from: aClassScope.
	
	"Priority is the value to be greater than all extensions group 
	in case when they are in same list (when extended class is selected)"
	^self named: aPackage name priority: 8.9 on: methodQuery
]

{ #category : #operations }
ClyExternalPackageMethodGroup >> importMethod: aMethod [ 
	super importMethod: aMethod.
	
	self package addMethod: aMethod.	
	aMethod tagWith: ('*', self package name) asSymbol
]

{ #category : #accessing }
ClyExternalPackageMethodGroup >> package [
	^ methodQuery package
]
