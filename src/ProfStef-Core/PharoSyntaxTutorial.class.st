"
The default Pharo tutorial to learn the Pharo syntax
"
Class {
	#name : #PharoSyntaxTutorial,
	#superclass : #AbstractTutorial,
	#category : #ProfStef-Core
}

{ #category : #lessons }
PharoSyntaxTutorial >> basicTypesArray [
	^ Lesson
title: 'Basic types: Array' 
lesson: 
'"Literal arrays are created at parse time:"

#(1 2 3).

#( 1 2 3 #(4 5 6)) size.

#(1 2 4) isEmpty.

#(1 2 3) first.

#(''hello'' ''World'') at: 2 put: ''Pharo''; yourself.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> basicTypesCharacters [
	^ Lesson
title: 'Basic types: Characters' 
lesson: 
'"A Character can be instantiated using $ operator:"

$A.

$A class.

$B charCode.

Character cr.

Character space.

"You can print all 256 characters of the ASCII extended set:"

Character allByteCharacters.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> basicTypesDynamicArray [
	^ Lesson
title: 'Basic types: Dynamic Array' 
lesson: 
'"Dynamic Arrays are created at execution time:"

{ (2+3) . (6*6) }.

{ (2+3) . (6*6) . ''hello'', '' Stef''} size.


{ ProfStef } first next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> basicTypesNumbers [
	^ Lesson
title: 'Basic types: Numbers' 
lesson: 
'"You now know how to execute Pharo code. 

Now let''s talk about basic objects.

1, 2, 100, 2/3 ... are Numbers, and respond to many messages evaluating mathematical expressions.
Evaluate these ones:"

2.

20 factorial.

1000 factorial / 999 factorial.

(1/3).

(1/3) + (4/5).

(1/3) asFloat.

1 class.

1 class maxVal class.

(1 class maxVal + 1) class.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> basicTypesString [
	^ Lesson
title: 'Basic types: Strings' 
lesson: 
'"A String is a collection of characters. Use single quotes to create a String object. Print these expressions:"

''ProfStef''.

''ProfStef'' size.

''abc'' asUppercase.

''Hello World'' reverse. 

"You can access each character using at: message"

''ProfStef'' at: 1.

"String concatenation uses the comma operator:"

''ProfStef'', '' is cool''.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> basicTypesSymbol [
	^ Lesson
title: 'Basic types: Symbols' 
lesson: 
'"A Symbol is a String which is guaranteed to be globally unique. 

There is one and only one Symbol #ProfStef. There may be several ''ProfStef'' String objects.

(Message == returns true if the two objects are the SAME)"

''ProfStef'' asSymbol.

#ProfStef asString.

(2 asString) == (2 asString).

(2 asString) asSymbol == (2 asString) asSymbol.


(Smalltalk globals at: #ProfStef) next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> blocks [
	^ Lesson
title: 'Blocks' 
lesson: 
'"Cascade is cool ! Let''s talk about blocks.

Blocks are anonymous methods that can be stored into variables and executed on demand.

Blocks are delimited by square brackets: []"

[Workspace open].

"does not open a Browser because the block is not executed.

Here is a block that adds 2 to its argument (its argument is named x):"

[:x | x+2].

"We can execute a block by sending it value messages."

[:x | x+2] value: 5.

[Workspace open] value.

[:x | x+2] value: 10.

[:x :y| x + y] value:3 value:5.

[ProfStef next] value.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> blocksAssignation [
	^ Lesson
title: 'Block assignation' 
lesson: 
'"Blocks can be assigned to a variable then executed later.

Note that |b| is the declaration of a variable named ''b'' and that '':='' assigns a value to a variable.

Select the three lines then Print It:"

|b|
b := [:x | x+2].
b value: 12.


ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> conditionals [
	^ Lesson
title: 'Conditionals' 
lesson: 
'"Conditionals are just messages sent to Boolean objects"

1 < 2
  ifTrue: [100]
  ifFalse: [42].

"Here the message is ifTrue:ifFalse

Try this:"

Transcript open.

3 > 10 
	ifTrue: [Transcript show: ''maybe there''''s a bug ....'']
	ifFalse: [Transcript show: ''No : 3 is less than 10''].

3 = 3 ifTrue: [ProfStef next].'.
]

{ #category : #lessons }
PharoSyntaxTutorial >> debugger [
	^ Lesson
title: 'Debugger' 
lesson: '"The Debugger may be the most famous tool of Smalltalk environments. It will open as soon as an unmanaged Exception occurs. 

The following code will open the debugger on the message stack, select PharoSyntaxTutorial>>divideTwoByZero".

PharoSyntaxTutorial new divideTwoByZero. '
]

{ #category : #interactive }
PharoSyntaxTutorial >> divideTwoByZero [
			2/0.

			"Oups! 2/0 raises a ZeroDivide exception.  So the debugger opens to let you fix the code.

			- Remove the line of code above.
			- Right-click and select 'Accept' to compile the new version of the method
			- click the button 'Proceed' to continue execution.
			".
			ProfStef next. 
]

{ #category : #lessons }
PharoSyntaxTutorial >> doingVSPrinting [ 
	^ Lesson
title: 'Doing' 
lesson: 
'"Cool ! (I like to say Cooool :) ). You''ve just executed a Pharo expression. More precisely, you sent the message ''next'' to ProfStef class (it''s me !).

Note you can run this tutorial again by evaluating: ''ProfStef go''. 
''ProfStef previous'' returns to the previous lesson.

You can also Do It using the keyboard shortcut ''ALT d''
(this varies according to your operating system/computer: it can be ''CMD d'' or ''CTRL d''). 

Try to evaluate these expressions:"

Workspace open.

SmalltalkImage current aboutThisSystem.

"Then go to the next lesson:"

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> inspecting [
	^ Lesson
title: 'Inspecting' 
lesson: 
'"Now you''re a Do It and Print It master ! Let''s talk about inspecting. It''s a Do It which opens an Inspector on the result of evaluating the expression you''ve selected. 
The Inspector is a tool that allows you to have a look inside an object.

For example, select the text below, open the menu and click on ''inspect it (i)'':"

1 / 2.

"You''ve seen the letter ''i'' between parentheses next to ''inspect it'' ? It indicates the ALT- (or CMD- or CTRL-) shortcut to execute this command.

Try ALT-i (or CMD-i or CTRL-i) on the following expressions:"

DateAndTime today.

Float pi.

SystemVersion current.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> instantiation [
	^ Lesson
title: 'Instantiation' 
lesson: 
'"Objects are instances of their class. Usually, we send the message #new to a class for creating an instance of this class.

The message #allInstances sent to a class answers an Array with all instances of this class.

For example, let''s look at how many instances of SimpleButtonMorph exist:"

SimpleButtonMorph allInstances size.

"Now create a new instance of it:"

SimpleButtonMorph new
	label: ''A nice button'';
	openCenteredInWorld.

"See the button centered on the world ? The list of all instances should contains one more instance:"

SimpleButtonMorph allInstances size.

"Let''s play with it:"

SimpleButtonMorph allInstances last 
	label: ''ProfStef is cooooool !'';
	color: Color cyan.
	
"Let''s delete it and ask the system to clean the memory:"

SimpleButtonMorph allInstances last delete.
Smalltalk garbageCollect.
SimpleButtonMorph allInstances size.

"Click on the button to go to next lesson:"

SimpleButtonMorph new
	label: ''Go to next lesson'';
	target: [ProfStef next. 
			SimpleButtonMorph allInstances last delete];
	actionSelector: #value;
	openCenteredInWorld.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> iterators [
	^ Lesson
title: 'Iterators' 
lesson: 
'"The message do: is sent to a collection of objects (Array, Set, OrderedCollection), evaluating the block for each element.

Here we want to print all the numbers on the Transcript (a console)"

#(11 38 3 -2 10) do: [:each |
     Transcript show: each printString; cr].

"Some other really nice iterators"

#(11 38 3 -2 10) collect: [:each | each abs].

#(11 38 3 -2 10) collect: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each > 10].

#(11 38 3 -2 10) reject: [:each | each > 10].

#(11 38 3 -2 10) 
     do: [:each | Transcript show: each printString]
     separatedBy: [Transcript show: ''.''].


ProfStef allInstances do: [:aPharoTutorial | aPharoTutorial next].'
]

{ #category : #lessons }
PharoSyntaxTutorial >> loops [
	^ Lesson
title: 'Loops' 
lesson: 
'"Loops are high-level collection iterators, implemented as regular methods."

"Basic loops:
  to:do:
  to:by:do"

1 to: 100 do:
  [:i | Transcript show: i asString; cr ].

1 to: 100 by: 3 do: [:i | Transcript show: i asString; cr].

100 to: 0 by: -2 do: 
    [:i | Transcript show: i asString; cr].

1 to: 1 do: [:i | ProfStef next].'
]

{ #category : #lessons }
PharoSyntaxTutorial >> mathematicalPrecedence [
	^ Lesson
title: 'Mathematical precedence'
lesson: 
'"Traditional precedence rules from mathematics do not apply in Pharo."

2 * 10 + 2.

"Here the message * is sent to 2, which answers 20, then 20 receive the message +

Remember that all messages always follow a simple left-to-right precedence rule, * without exceptions *."

2 + 2 * 10.

2 + (2 * 10).

8 - 5 / 2.

(8 - 5) / 2.

8 - (5 / 2).

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> messageSyntaxBinary [
	^ Lesson
title: 'Message syntax: Binary messages' 
lesson: 
'"Binary messages have the following form:
    anObject + anotherObject"

3 * 2.

Date today + 3 weeks.

false | false.

true & true.

true & false.

10 @ 100.

10 <= 12.

''ab'', ''cd''.

Date today < Date yesterday.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> messageSyntaxCascade [
	^ Lesson
title: 'Message syntax: Cascade' 
lesson: 
'"; is the cascade operator. It''s useful to send message to the SAME receiver
Open a Transcript (console):"

Transcript open.

"Then:"

Transcript show: ''hello''.
Transcript show: ''Pharo''.
Transcript cr.

"is equivalent to:"

Transcript 
	   show: ''hello'';
	   show: ''Pharo'' ;
	   cr.

"Try to go to the next lesson with a cascade of two ''next'' messages:"

ProfStef'.
]

{ #category : #lessons }
PharoSyntaxTutorial >> messageSyntaxCascadeShouldNotBeHere [
	^ Lesson
title: 'Lost ?' 
lesson: 
'"Hey, you should not be here !! 

Go back and use a cascade !"

ProfStef previous.'.
]

{ #category : #lessons }
PharoSyntaxTutorial >> messageSyntaxExecutionOrder [
	^ Lesson
title: 'Message syntax: Execution order' 
lesson: 
'"Unary messages are executed first, then binary messages and finally keyword messages:
    Unary > Binary > Keywords"

2 + 3 squared.

2 raisedTo: 3 + 2.
  
(0@0) class.

0@0 corner: 100@200.

(0@0 corner: 100@200) class.

"Between messages of similar precedence, expressions are executed from left to right"

-3 abs negated reciprocal.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> messageSyntaxExecutionOrderParentheses [
	^ Lesson
title: 'Message syntax: Parentheses'
lesson: 
'"Use parentheses to change order of evaluation"

(2 + 3) squared.

(2 raisedTo: 3) + 2.

(0@0 extent: 100@200) bottomRight.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> messageSyntaxKeyword [
	^ Lesson
title: 'Message syntax: Keyword messages' 
lesson: 
'"Keyword Messages are messages with arguments. They have the following form:
    anObject akey: anotherObject akey2: anotherObject2"

4 between: 0 and: 10.

"The message is between:and: sent to the Number 4"

1 max: 3.

Color r:1 g:0 b:0.

"The message is r:g:b: implemented on class Color. Note you can also write"

Color
	r:1
	g:1
	b:0.
	
ProfStef perform: #next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> messageSyntaxUnary [
	^ Lesson
title: 'Message syntax: Unary messages' 
lesson: 
'"Messages are sent to objects. There are three types of message: Unary, Binary and Keyword.

Unary messages have the following form:
    anObject aMessage 

You''ve already sent unary messages. For example:"

1 class.

false not.

Time now.

Date today.

Float pi.

"And of course: "

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> pharoEnvironment [
	^ Lesson
title: 'Pharo environment' 
lesson: 
'"Pharo is full of objects. There are windows, text, numbers, dates, colors, points and much more. You can interact with objects in a much more direct way than is possible with other programming languages.

Every object understands the message ''explore''. As a result, you get an Explorer window that shows details about the object."

Date today explore.

"This shows that the date object consists of a point in time (start) and a duration (one day long)."

ProfStef explore.

"You see, ProfStef class has a lot of objects. Let''s take a look at my code:"

ProfStef browse.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> printing [ 
	^ Lesson
title: 'Printing' 
lesson: 
'"Now you''re a Do It master ! Let''s talk about printing. It''s a Do It which prints the result next to the expression you''ve selected.
For example, select the text below, open the menu and click on ''print it (p)'':"

1 + 2.

"You''ve seen the letter ''p'' between parentheses next to ''print it'' ? It indicates the ALT- (or CMD- or CTRL-) shortcut to execute this command.

Try ALT-p (or CMD-p or CTRL-p) on the following expressions:"

Date today.

Time now.

"The result is selected, so you can erase it using the backspace key. Try it !"

SmalltalkImage current datedVersion.

ProfStef next.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> reflection [
	^ Lesson
title: 'Reflection' 
lesson: 
'"You can inspect and change the system at runtime.

Take a look at method #ifFalse:ifTrue: source code of class True:"

(True>>#ifFalse:ifTrue:) definition.

"Or just its comment:"

(True>>#ifFalse:ifTrue:) comment.

"Here''s all the methods I implement:"

ProfStef selectors.


"Let''s create a new method to go to the next lesson:"

ProfStef class compile:''goToNextLesson
  self next''.

"Wow ! I can''t wait to use my new method ! "

ProfStef goToNextLesson.'
]

{ #category : #lessons }
PharoSyntaxTutorial >> reflectionContinued [
	^ Lesson
title: 'Reflection continued' 
lesson: 
'"So cool, isn''t it ?  Before going further, let''s remove this method:"

ProfStef respondsTo: #goToNextLesson.

ProfStef class removeSelector: #goToNextLesson.

ProfStef respondsTo: #goToNextLesson.


"Then move forward:"

ProfStef default executeMethod: (ProfStef lookupSelector:#next).'
]

{ #category : #lessons }
PharoSyntaxTutorial >> theEnd [
	^ Lesson
title: 'Tutorial done !' 
lesson: 
'"This tutorial is done. Enjoy programming with Pharo. 

Don''t forget to read ''Pharo By Example'' found here:  

	http://pharobyexample.org/

You can run this tutorial again by evaluating:"

ProfStef go.

"Do you want to create your own interactive tutorial with ProfStef? That''s very easy!!  How ? There''s a ProfStef interactive tutorial for that :D
Just evaluate the following code:"

ProfStef goOn: HowToMakeYourOwnTutorial

"See you soon !"
'
]

{ #category : #tutorial }
PharoSyntaxTutorial >> tutorial [
^ #(
welcome
doingVSPrinting
printing
inspecting

basicTypesNumbers
basicTypesCharacters
basicTypesString
basicTypesSymbol
basicTypesArray
basicTypesDynamicArray

messageSyntaxUnary
messageSyntaxBinary
messageSyntaxKeyword
messageSyntaxExecutionOrder
messageSyntaxExecutionOrderParentheses
mathematicalPrecedence
messageSyntaxCascade
messageSyntaxCascadeShouldNotBeHere

blocks
blocksAssignation
conditionals
loops
iterators

instantiation

reflection
reflectionContinued

pharoEnvironment
debugger

theEnd
)
]

{ #category : #lessons }
PharoSyntaxTutorial >> welcome [
	^ Lesson
title: 'Welcome' 
lesson: 
'"Hello! I''m Professor Stef.

You must want me to help you learn Pharo.

So let''s go to the first lesson.  Select the text below, right-click and choose ''do it (d)''"

ProfStef next.'
]
