Class {
	#name : #MTDevelopmentWorkflowTest,
	#superclass : #TestCase,
	#instVars : [
		'project',
		'classFactory',
		'class',
		'visitor'
	],
	#category : #'Versionner-Tests-Core-DependenciesModel'
}

{ #category : #tests }
MTDevelopmentWorkflowTest >> testCreateInitialDevelopment [
	|  version configuration |

	project := MTProject newNamed: 'Z' withInitialVersion: '0.1'  inRepository: 'http://smalltalkhub.com/mc/demarey/Versionner/main'.
	configuration := project configurationClass.
	
	visitor := MCModel2MTModelVisitor new.
	self shouldnt: [ version := configuration project version: #development]
		 raise: MetacelloSymbolicVersionDoesNotExistError
		description: 'Cannot find the #development symbolic version in the metacello description'.
	
	self assert: configuration project development versionString
		equals: '0.1-baseline'.
	
	"configuration removeFromSystem."
]

{ #category : #tests }
MTDevelopmentWorkflowTest >> testNewProjectWithInitialVersionInRepository [
	|  projectName configClass |

	projectName := 'VersionnerTest'.
	project := MTProject newNamed: projectName withInitialVersion: '0.1'  inRepository: ''.
	
	self assert: (project configurationClass notNil) description: 'Project configuration class is nil.'.
	self shouldnt: [configClass := Smalltalk globals at: (project configurationClass name)]
		raise: KeyNotFound
		description: 'Cannot find the Configuration class in the image.'.
	self assert: project name equals: projectName.
	
	configClass removeFromSystem.
	
]

{ #category : #tests }
MTDevelopmentWorkflowTest >> testNextDevelopmentVersionString [
	| workflow |
	
	workflow := MTDevelopmentWorkfow new.
	self assert: (workflow nextDevelopmentVersionString: '1.0-baseline') equals: '1.1-baseline'.
	self assert: (workflow nextDevelopmentVersionString: '1.0') equals: '1.1-baseline'.
	self assert: (workflow nextDevelopmentVersionString: 'baseline-0.9') equals: '0.10-baseline'.
]

{ #category : #tests }
MTDevelopmentWorkflowTest >> testStandardizeDevVersionString [
	| workflow |
	
	workflow := MTDevelopmentWorkfow new.
	self assert: (workflow standardizeDevVersionString: '1.0-baseline') equals: '1.0-baseline'.
	self assert: (workflow standardizeDevVersionString: 'baseline-1.3') equals: '1.3-baseline'.
]
