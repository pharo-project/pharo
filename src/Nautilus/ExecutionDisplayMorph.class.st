"
Inidicator morph for the Nautilus plugin ExecutionDisplayPlugin.
I change my color on invokation of the registered method.

My color (the execution indicator) fades away over time.
"
Class {
	#name : #ExecutionDisplayMorph,
	#superclass : #BorderedMorph,
	#category : #'Nautilus-Plugins-Examples'
}

{ #category : #private }
ExecutionDisplayMorph >> nextAlphaStep [
	^ 0.0 max: self color alpha - 0.15
]

{ #category : #'meta link action' }
ExecutionDisplayMorph >> noteExecuted [
	self color: Color red.
]

{ #category : #'stepping and presenter' }
ExecutionDisplayMorph >> step [
	self color: (self color alpha: self nextAlphaStep)
]

{ #category : #'model - stepping' }
ExecutionDisplayMorph >> stepTime [
	^ 200
]

{ #category : #'stepping and presenter' }
ExecutionDisplayMorph >> wantsSteps [
	^ true
]
