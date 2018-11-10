"
I provide 'extensions' method group
"
Class {
	#name : #ClyExtendedMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClyExtendedMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	
	| subgroupsQuery |
	subgroupsQuery := ClyExtendingPackages 
		from: aClassScope as: ClyGroupedExtendingPackages new.
		
	^ClyMethodGroup 
		named: 'extensions' priority: 9
		on: aMethodQuery withSubgroupsFrom: subgroupsQuery
]

{ #category : #'building groups' }
ClyExtendedMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyAllExtensionMethods from: aClassScope
]
