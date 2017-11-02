"
Update the current development version with infomation provided by the project.

"
Class {
	#name : #VSUpdateDevelopmentVersionCommand,
	#superclass : #VSDevCycleCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #execute }
VSUpdateDevelopmentVersionCommand >> execute [

	self ensureDevelopmentVersionExists.
	self ensureDevelopmentVersionIsBaseline.
	
	workflow updateDevelopment

]

{ #category : #accessing }
VSUpdateDevelopmentVersionCommand >> title [
	^ 'Update development version'
]
