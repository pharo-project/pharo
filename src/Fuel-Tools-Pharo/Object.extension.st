Extension { #name : #Object }

{ #category : #'*Fuel-Tools-Pharo' }
Object >> fuelOut [
	| target |
	target := FileLocator imageDirectory / self printString asFileName, 'fuel'.
	self serializeToFileNamed: target nextVersion fullName
]
