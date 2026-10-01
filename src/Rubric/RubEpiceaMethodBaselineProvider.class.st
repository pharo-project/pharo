"
I provide the baseline source code for a method by querying the Epicea log.
I look for the last EpMethodModification event for the given method
and return its oldSourceCode as the baseline.

If no event is found in the log, I answer nil (letting the fallback chain handle it).
"
Class {
	#name : 'RubEpiceaMethodBaselineProvider',
	#superclass : 'RubMethodBaselineProvider',
	#category : 'Rubric-Editing-Widgets',
	#package : 'Rubric',
	#tag : 'Editing-Widgets'
}

{ #category : 'accessing' }
RubEpiceaMethodBaselineProvider >> baselineForMethod: aCompiledMethod [

	| className selector entry |

	EpMonitor hasCurrent ifFalse: [ ^ nil ].

	className := aCompiledMethod methodClass instanceSide name.
	selector := aCompiledMethod selector.

	entry := EpMonitor current log fromHeadDetect: [ :each | | event |
		event := each content.
		(event isKindOf: EpMethodModification) and: [
			(event behaviorAffectedName = className) and: [ event methodAffectedSelector = selector ] ] ]
		         ifNotFound: [ nil ].

	entry ifNil: [ ^ nil ].

	^ entry content oldSourceCode
]
