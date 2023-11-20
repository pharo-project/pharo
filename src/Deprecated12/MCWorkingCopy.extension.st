Extension { #name : 'MCWorkingCopy' }

{ #category : '*Deprecated12' }
MCWorkingCopy class >> allManagers [

	self deprecated: 'Use #allWorkingCopies instead' transformWith: '`@rcv allManagers' -> '`@rcv allWorkingCopies'.
	^ self allWorkingCopies
]

{ #category : '*Deprecated12' }
MCWorkingCopy >> packageSet [
	self deprecated: 'I represent a unique package, thus I should not use a package set. Use #systemPackage instead.'.
	^ package packageSet
]
