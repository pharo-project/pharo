"
I annotate table decorators (ClyTableDecorator subclasses) to define where decorator should be used, in what context of selected items.

For example following method will declare that classes in any browsers should be decorated with abstract class decorator:

ClyAbstractClassTableDecorator class>>classDecorationStrategy
	<classAnnotation>
	^ClyTableDecorationStrategy for: ClyClass asCalypsoItemContext
	
It only declares where to use decorator but decorator itself can define extra conditions to check that given item is actually should be decorated
	annotatedClass wantsDecorateTableCellInContext: aBrowserItemContext
	
I sort my registered instances according to ascending priority. It is opposite to default order of annotation registry. 
Idea that most prioritized decorator should be able override visual effects from less prioritized decorators. And to achieve it I just enumerate instances in described order: asceding priority, which evaluates most prioritized decorator at last order.

Important notice. The actual priority is defined by decorator classes in class side method #priority. I retrieve them when annotation is created
"
Class {
	#name : #ClyTableDecorationStrategy,
	#superclass : #ClassAnnotation,
	#category : #'Calypso-Browser-Table'
}

{ #category : #private }
ClyTableDecorationStrategy class >> createContainerForRegistry [
	"Decoration with more priority should override applied changes of table row 
	which was produced by less prioritized decorations.
	It can be done if most prioritized decorator will be applied on the table at last order.
	Here we revert annotation enumeration logic by managing registry in opposite order
	according to described idea"
	
	^SortedCollection sortBlock: #priority ascending
]

{ #category : #accessing }
ClyTableDecorationStrategy >> annotatedClass: aTableDecoratorClass [
	super annotatedClass: aTableDecoratorClass.
	
	priority := aTableDecoratorClass priority
]

{ #category : #decoration }
ClyTableDecorationStrategy >> decorateMainTableCell: anItemCellMorph inContext: itemContext [ 

	annotatedClass decorateMainTableCell: anItemCellMorph inContext: itemContext
]

{ #category : #decoration }
ClyTableDecorationStrategy >> decorateTableCell: anItemCellMorph inContext: itemContext [ 

	annotatedClass decorateTableCell: anItemCellMorph inContext: itemContext
]

{ #category : #testing }
ClyTableDecorationStrategy >> isActiveInContext: aBrowserItemContext [
	^(super isActiveInContext: aBrowserItemContext)
		and: [ annotatedClass wantsDecorateTableCellInContext: aBrowserItemContext ]
]

{ #category : #testing }
ClyTableDecorationStrategy >> isForbidden [
	^annotatedClass isAbstract
]

{ #category : #printing }
ClyTableDecorationStrategy >> printOn: aStream [
	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: (annotatedClass ifNil: [ 'detached' ] ifNotNil: [ annotatedClass name]);
		nextPutAll: '; ';
		nextPutAll: priority asString;
		nextPut: $)
]
