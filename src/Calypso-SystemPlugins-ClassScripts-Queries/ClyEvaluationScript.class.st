"
I represent class side methods with pragma <script>
"
Class {
	#name : #ClyEvaluationScript,
	#superclass : #ClyClassScript,
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'script detection' }
ClyEvaluationScript class >> isImplementedByMethod: aMethod [
	^ aMethod hasPragmaNamed: #script
]
