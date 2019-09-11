Extension { #name : #SpInterpreter }

{ #category : #'*Spec2-Deprecated' }
SpInterpreter class >> bindings: anAdapterBinding [
	self deprecated: 'Never worked.'
]

{ #category : #'*Spec2-Deprecated' }
SpInterpreter class >> defaultBindings [
	self deprecated: 'Use #bindings instead.' transformWith: '`@receiver defaultBindings' -> '`@receiver bindings'.
	^ self bindings
]

{ #category : #'*Spec2-Deprecated' }
SpInterpreter class >> hardResetBindings [
	self deprecated: 'Never worked'
]

{ #category : #'*Spec2-Deprecated' }
SpInterpreter class >> interpretASpec: aSpec model: aPresenter [
	self
		deprecated: 'Use `interpretASpec: aSpec presenter: aPresenter` instead'
		transformWith: '`@receiver interpretASpec: `@statements1 model: `@statements2' -> '`@receiver interpretASpec: `@statements1 presenter: `@statements2'.
		
	^ self interpretASpec: aSpec presenter: aPresenter
]

{ #category : #'*Spec2-Deprecated' }
SpInterpreter >> interpretASpec: aSpec model: aPresenter selector: aSelector [
	self
		deprecated: 'Use `interpretASpec: aSpec presenter: aPresenter` instead'
		transformWith: '`@receiver interpretASpec: `@statements1 model: `@statements2 selector: `@statements3' -> '`@receiver interpretASpec: `@statements1 presenter: `@statements2'.

	^ self interpretASpec: aSpec presenter: aPresenter
]

{ #category : #'*Spec2-Deprecated' }
SpInterpreter class >> interpretASpec: aSpec model: aPresenter selector: aSelector [
	self
		deprecated: 'Use `interpretASpec: aSpec presenter: aPresenter` instead'
		transformWith: '`@receiver interpretASpec: `@statements1 model: `@statements2 selector: `@statements3' -> '`@receiver interpretASpec: `@statements1 presenter: `@statements2'.
		
	^ self interpretASpec: aSpec presenter: aPresenter
]

{ #category : #'*Spec2-Deprecated' }
SpInterpreter >> model [
	self deprecated: 'Use #presenter instead' transformWith: '`@receiver model' -> '`@receiver presenter'.
	^ self presenter
]

{ #category : #'*Spec2-Deprecated' }
SpInterpreter >> model: anObject [
	self deprecated: 'Use #presenter: instead' transformWith: '`@receiver model: `@statment1' -> '`@receiver presenter: `@statment1'.
	self presenter: anObject
]
