"
i represent stroke paint for balloon backend
"
Class {
	#name : #AthensBalloonStrokePaint,
	#superclass : #AthensStrokePaint,
	#category : #'Athens-Balloon-Paint'
}

{ #category : #rendering }
AthensBalloonStrokePaint >> athensFillPath: aPath on: aCanvas [

	^ fillPaint strokePath: aPath on: aCanvas strokeWidth: width
]

{ #category : #rendering }
AthensBalloonStrokePaint >> athensFillRectangle: aRect on: aCanvas [

	^ fillPaint strokeRect: aRect on: aCanvas strokeWidth: width
]

{ #category : #'setting - cap styles' }
AthensBalloonStrokePaint >> capButt [
	self flag: #pharoTodo
]

{ #category : #'setting - cap styles' }
AthensBalloonStrokePaint >> capRound [
	self flag: #pharoTodo
]

{ #category : #'setting - cap styles' }
AthensBalloonStrokePaint >> capSquare [
	self flag: #pharoTodo
]

{ #category : #'setting - join styles' }
AthensBalloonStrokePaint >> joinBevel [
	self flag: #pharoTodo
]

{ #category : #'setting - join styles' }
AthensBalloonStrokePaint >> joinMiter [
	self flag: #pharoTodo
]

{ #category : #'setting - join styles' }
AthensBalloonStrokePaint >> joinRound [
	self flag: #pharoTodo
]
