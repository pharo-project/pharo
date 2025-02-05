"
The default Pharo tutorial to learn the Pharo syntax
"
Class {
	#name : 'PharoSyntaxTutorial',
	#superclass : 'AbstractTutorial',
	#category : 'ProfStef-Core',
	#package : 'ProfStef-Core'
}

{ #category : 'lessons' }
PharoSyntaxTutorial >> basicTypesArray [
	^ Lesson
title: 'Basic types: Array'
lesson:
'"Pharo supports arrays (sequences of objects). Arrays can be created in different ways: 
- Sending the nessage new: to the class Array, 
- Using the literal syntax #(1 2 3), and 
- Using the dynamic syntax { 1 . 2 . 3 }

Here we look at literal arrays.
Literal arrays are created at parse time and are read-only.
They can only contain literal objects (objects created by the parser, it means without messages)"

#(1 2 3)

#(1 2 3 #(4 5 6)) size

#(1 2 4) isEmpty

#(1 2 3) first

#(''hello'' ''World'') at: 2

"You can modify a copy"
#(''hello'' ''World'') copy at: 2 put: ''Pharo''; yourself



ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> basicTypesCharacters [
	^ Lesson
title: 'Basic types: Characters'
lesson:
'"A Character can be instantiated using $ operator:"

$A

$A class

$B charCode

"To get special characters we send message to the Character class."

Character cr

Character space

"You can print all 256 characters of the ASCII extended set:"

Character allByteCharacters

ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> basicTypesDynamicArray [
	^ Lesson
title: 'Basic types: Dynamic Array'
lesson:
'"The other syntax to define array is called Dynamic Arrays.
Contrary to literal arrays they are created at execution time:"

"The following creates an array of 6 nil elements"

Array new: 6

"The following two lines create the same array. On expressed as message to the class Array and one using the { } syntax."

Array with: (2+3) with: (6*6)

{ (2+3) . (6*6) }.

"Guess the result before trying and understand why you get that result"

{ (2+3) . (6*6) . ''hello'', '' Stef''} size.


"Compare the two following expression results:"

{  Point new setX: 100 setY: 200 }

#({  Point new setX: 100 setY: 200 })



{ ProfStef } first next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> basicTypesNumbers [
	^ Lesson
title: 'Basic types: Numbers'
lesson:
'"You now know how to execute Pharo code.

Now let''s talk about basic objects.

1, 2, 100, 2/3 ... are Numbers, and respond to many messages evaluating mathematical expressions.
Evaluate (Print It) these ones:"

	2

	20 factorial

	1000 factorial / 999 factorial

	(1/3)

	(1/3) + (4/5)

	(1/3) asFloat

	1 class

	1 class maxVal class

	(1 class maxVal + 1) class

ProfStef next.'
]

{ #category : 'lessons' }
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

{ #category : 'lessons' }
PharoSyntaxTutorial >> basicTypesSymbol [
	^ Lesson
title: 'Basic types: Symbols'
lesson:
'"A Symbol is a String which is guaranteed to be globally unique.
A symbol starts with # sign and followed by characters.

For example, there is one and only one Symbol #ProfStef. Even here #ProfStef #ProfStef refer to the same unique symbol whose representation is ''ProfStef''. 

There may be several ''ProfStef'' String objects.

The message == returns true if the two objects are the SAME object.

Try the following expression: "

''ProfStef'' asSymbol

#ProfStef asString

"The following expressions returns different results. Do you guess why?"


(2 asString) == (2 asString)

(2 asString asSymbol) == (2 asString asSymbol)



"This is because the message asString returns a new string. So the first expression checks that two different strings are the same objects: clearly this is false.  

The message asSymbol turn the receiver in an unique symbol (the same if already exists).
So the second expression creates new strings but turns these strings into a unique symbol
using the message asSymbol. Then it compares the symbols are they refer to exactly the same object so the results is true."


ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> blocks [
	^ Lesson
title: 'Blocks'
lesson:
'"Cascade is cool! Let''s talk about blocks.

Blocks are anonymous methods that can be stored into variables, passed as method arguments and executed on demand.

Blocks are delimited by square brackets: [].

There is a difference between defining a block and executing a block. Let us have a look.
Execute the following expression:
"

[ StPlaygroundPresenter open ]

"It does not open a Browser because the block is defined but not executed. 
To execute a block we should send it the message value as follows: "


[ StPlaygroundPresenter open ] value



"
Let us take another example. Here is a block that adds 2 to its argument (its argument is named x):"

[ :x | x + 2 ]

"We can execute a block by sending the message value: because the block has one argument."

[ :x | x + 2 ] value: 5


[ :x | x + 2 ] value: 10

"We can have block with two or more arguments and we will use the value:value: message as follows:"


[ :x :y | x + y] value: 3 value: 5

[ProfStef next] value'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> blocksAssignation [
	^ Lesson
title: 'Block assignment'
lesson:
'"Blocks can be assigned to a variable then executed later.

Note that | b | is the declaration of a variable named ''b'' and that '':='' assigns a value to a variable.

Select the three lines then Print it:"


| b |
b := [ :x | x + 2 ].
b value: 12.


"Execute the following expression to pass to the next lesson."


| b | 
b := [ ProfStef next ].
b value

'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> conditionals [
	^ Lesson
title: 'Conditionals'
lesson:
'"Conditionals are just messages sent to Boolean objects.
Here is a simple conditional."

1 < 2
 	ifTrue: [ 100 ]
	ifFalse: [ 42 ]

"Here the message is ifTrue:ifFalse: sent to a Boolean (the result of the expression 1 < 2)

There are several messages to express conditions: ifTrue:ifFalse: and ifFalse:ifTrue:.
The following expression is the same as the previous one, it is handy because sometimes 
the developer wants to stress an aspect."

1 < 2
	ifFalse: [ 42 ]
	ifTrue: [ 100 ]

"In addition Pharo offers the two messages ifTrue: and ifFalse:.
The following expression returns 33."

1 < 10 
	ifTrue: [  33 ]  


"Note that the following expressions are not equivalent"

| c |
c := 0.
(c := c + 1) < 1
	ifFalse: [ 42 ]
	ifTrue: [ 100 ]


| c |
c := 0.
(c := c + 1) < 1
	ifFalse: [ 42 ].
(c := c + 1) < 1
	ifTrue: [ 100 ]

"Why? Because in the second case the condition is executed twice. While in the first one the condition is only invoked once.
This is the same in any programming language"


3 = 3 ifTrue: [ ProfStef next ]'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> debugger [
	^ Lesson
title: 'Debugger'
lesson: '"The Debugger may be the most famous tool of Pharo environment. It will open as soon as an unmanaged Exception occurs.

The following code will open the debugger on the message stack."

1 / 0 

"Close the debugger. 

Now you can also invoke a debugger by inserting a breakpoint in your code.
There are different breakpoints but a simple one is self halt.

When you execute the following expression you will get another debugger, click on the line PharoSyntaxTutorial>>divideTwoByZero and follow the instruction within the debugger itself."

PharoSyntaxTutorial new divideTwoByZero.'
]

{ #category : 'interactive' }
PharoSyntaxTutorial >> divideTwoByZero [
	

	"Oops! 2/0 raises a ZeroDivide exception. So the debugger opens to let you fix the code.

	- Remove the line of code above.
	- Right-click and select 'Accept' to compile the new version of the method
	- click the button 'Proceed' to continue execution."

	ProfStef next
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> doingVSPrinting [
	^ Lesson
title: 'Doing'
lesson:
'"Cool! (I like to say Cooool :) ). You''ve just executed a Pharo expression. More precisely, you sent the message ''next'' to ProfStef class (it''s me!).

Note you can run this tutorial again by evaluating: ''ProfStef go''.
''ProfStef previous'' returns to the previous lesson.

You can also Do it using the keyboard shortcut ''Ctrl + D'' (or ''Cmd + D'' on MacOS).

Try to evaluate these expressions:"

	StPlaygroundPresenter open.

	ProfStef openPharoZenWorkspace.

"Then go to the next lesson:"

ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> inspecting [
	^ Lesson
title: 'Inspecting'
lesson:
'"Now you''re a Do it and Print it Master! Let''s talk about inspecting. It''s a Do it which opens an Inspector on the result of evaluating the expression you''ve selected.
The Inspector is a super cool tool that allows you to have a look inside an object.

For example, select the text below, open the menu and click on ''Inspect it'':"

Date today

"You''ve seen the keyboard keys next to the ''Inspect it''? It indicates the Ctrl- (or Cmd-) shortcut to execute this command.

Try ''Ctrl + I'' (or ''Cmd + I'') on the following expressions:"

DateAndTime today

Float pi

SystemVersion current

ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> instantiation [
	^ Lesson
title: 'Instantiation'
lesson:
'"Objects are instances of their class. Usually, we send the message #new to a class for creating an instance of this class.

The message #allInstances sent to a class answers an Array with all instances of this class.

For example, let''s look at how many instances of SimpleButtonMorph exist. Please use only Do it or Print it on this page, not Inspect it."

SimpleButtonMorph allInstances size.

"Now create a new instance of it:"

SimpleButtonMorph new
	label: ''A nice button'';
	openCenteredInWorld.

"See the button centered on the world? The list of all instances should contains one more instance:"

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

{ #category : 'lessons' }
PharoSyntaxTutorial >> iterators [
	^ Lesson
title: 'Iterators'
lesson:
'"Pharo offers a large set of iterators. 

One of the iconic one is the message do:. It is sent to a collection of objects (Array, Set, OrderedCollection), executing the block for each element.

Here we want to print all the numbers on the Transcript (a console)"

Transcript open; clear. 
#(11 38 3 -2 10) do: [ :each | each traceCr ].


"Some other really nice iterators"

#(11 38 3 -2 10) collect: [ :each | each abs ].

#(11 38 3 -2 10) collect: [ :each | each odd ].

#(11 38 3 -2 10) select: [ :each | each odd ].

#(11 38 3 -2 10) select: [ :each | each > 10 ].

#(11 38 3 -2 10) reject: [ :each | each > 10 ].

#(11 38 3 -2 10)
     do: [ :each | each traceCr ]
     separatedBy: [ ''.'' traceCr ].

{ 1 } do: [ :each |  ProfStef next ]'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> loops [
	^ Lesson
title: 'Loops'
lesson:
'"
Pharo offers a large set of loops: basic loops, conditional loops, and iterators. 
In this lesson we focus on basic loops"

"Pharo offers several basic loops:
	timesRepeat:
  	to:do:
  	to:by:do:

Execute the following expression 
"

Transcript open; clear.

"Then try the following expressions:"

5 timesRepeat: [  ''A'' traceCr ].

1 to: 100 do: [ :i | i traceCr ].

1 to: 100 by: 3 do: [ :i | i traceCr ].

100 to: 0 by: -2 do: [ :i | i traceCr ].


"Then to pass to the next lesson."


1 to: 1 do: [ :i | ProfStef next ]'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> mathematicalPrecedence [
	^ Lesson
title: 'Mathematical precedence'
lesson:
'"Traditional precedence rules from mathematics do not apply in Pharo.
Let us have a look at the following expression"

2 + 10 * 2

"Here the message + is sent to 2, which answers 12, then 12 receives the message *, so we get 24.
Clearly this does not follow mathematical evaluation.
To get the mathematical result we should use parentheses as follows: "

2 + (10 * 2)


"Of course, sometimes this is not necessary to use parentheses as in the following two expressions that are equivalent:"

5 * 10 + 2
(5 * 10) + 2


"Remember that all messages always follow a simple left-to-right precedence rule, without any exceptions.
So try to guess the value returned by the following expressions:"

2 + 2 * 10

2 + (2 * 10)

8 - 5 / 2

(8 - 5) / 2

8 - (5 / 2)



ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageCanBeSeparatedByDot [
	^ Lesson
title: 'Execution in Sequence'
lesson:
'"You can execute multiple messages in sequence by separating them with a dot (.)"


"Here we open an inspector on the point created by (0@0 corner: 100@200) and after we open an empty playground. Select the two following lines."

(0@0 corner: 100@200) inspect.
StPlaygroundPresenter open

"Notice that the last sentence could finish by a period but this is optional since a dot is a separator,"

"Notice also that you can select a single line. You can also execute an expression finishing by a period. Try to select and execute each of the previous line one by one."

ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageSyntaxBinary [
	^ Lesson
title: 'Message syntax: Binary messages'
lesson:
'"Binary messages have the following form:

    anObject aBinarySelector anotherObject

where a binary selector is from a list of non alphabetical characters e.g. ~ = , < > + - / *
& @ %. A binary selector can be for example == or <="

3 * 2

Date today + 3 weeks

false | false

true & true

true & false

10 @ 100

10 <= 12

''ab'' , ''cd''

Date today < Date yesterday




ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageSyntaxCascade [
	^ Lesson
title: 'Message syntax: Cascade'
lesson:
'"; is the cascade operator. It''s useful to send message to the SAME receiver.

Open a Transcript und (console):"

Transcript open.
Transcript clear.

"Then:"

Transcript show: ''hello ''.
Transcript show: '' Pharo''.
Transcript cr.

"is equivalent to:"

Transcript
	   show: ''hello '';
	   show: '' Pharo'' ;
	   cr.

"The cascade is usefull because it helps not having to use a temporary variable."


"Try to go to the next lesson with a cascade of two ''next'' messages:"



ProfStef'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageSyntaxCascadeShouldNotBeHere [
	^ Lesson
title: 'Lost?'
lesson:
'"Hey, you should not be here!!

Go back and use a cascade!"

ProfStef previous.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageSyntaxExecutionOrder [
	^ Lesson
title: 'Message syntax: Execution order'
lesson:
'"
Pharo distinguishes three kinds of messages: unary, binary, keywords. This distinction is used to minimize the number of parentheses to be written. It is because there is a priority between the message kinds. 

Unary messages are executed first, then binary messages and finally keyword messages:

    Unary > Binary > Keywords"

"Make sure that you understand the order of which each expression is executed: "


2 + 3 squared

2 raisedTo: 3 + 2

(0@0) class

0@0 corner: 100@200

(0@0 corner: 100@200) class


"Between messages of the same precedence, expressions are executed from left to right"

-3 abs negated reciprocal.



ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageSyntaxExecutionOrderParentheses [
	^ Lesson
title: 'Message syntax: Parentheses'
lesson:
'"
Unary messages are executed before binary messages, and binary messages are executed before keywords. Messages of the same priority are evaluated from left to right.

Use parentheses to change order of evaluation.
Guess the result of the following expression. Follow the priority and the flow of execution.
"




(2 + 3) squared

(2 raisedTo: 3) + 2

(0@0 extent: 100@200) bottomRight




"Are you sure that you understand how message precedence works?
A bit... this is normal.

Let us try again. 
Do you see that some parentheses are unnecessary in the following expressions?"


(2 asString) = (2 asString)

(2 asString asSymbol) = (2 asString asSymbol)

"Remove some parentheses in the expressions of this lesson and try to guess the result you will get."



ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageSyntaxKeyword [
	^ Lesson
title: 'Message syntax: Keyword messages'
lesson:
'"Keyword messages are messages with arguments. They have the following form:

    anObject akey: anotherObject akey2: anotherObject2 "

4 between: 0 and: 10

"The message selector is between:and: sent to the Number 4 with 0 and 10 as argument."

"Identify the message selectors for the following expressions:"


1 max: 3

Color r: 1 g: 0 b: 0


"The message selector is r:g:b: implemented on class Color. Note you can also write"

Color
	r: 1
	g: 1
	b: 0


ProfStef perform: #next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> messageSyntaxUnary [
	^ Lesson
title: 'Message syntax: Unary messages'
lesson:
'"Messages are sent to objects. There are three types of message: unary, binary and keyword.

Unary messages have the following form:
    
	anObject aMessageSelector

You''ve already sent unary messages. For example:"

1 class

false not

Time now

Date today

Float pi



"And of course:"

ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> pharoEnvironment [
	^ Lesson
title: 'Pharo environment'
lesson:
'"Pharo is full of objects. There are windows, text, numbers, dates, colors, points and much more. You can interact with objects in a much more direct way than is possible with other programming languages.

Every object understands the message ''inspect''. As a result, you get an Inspector window that shows details about the object."

Date today inspect.

"This shows that the date object consists of a point in time (start) and a duration (one day long)."

ProfStef inspect.

"You see, ProfStef class has a lot of objects. Let''s take a look at my code:"

ProfStef browse.

ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> printing [
	^ Lesson
title: 'Printing'
lesson:
'"Now you''re a Do it Master! Let''s talk about printing. It''s a Do it which prints the result in a tooltip next to the expression you''ve selected.
For example, select the text below, open the menu and click on ''Print it'':"

	1 + 2.

"You''ve seen the keyboard keys next to the ''Print it''? It indicates the Ctrl- (or Cmd-) shortcut to execute this command.

Try ''Ctrl + P'' (or ''Cmd + P'') on the following expressions:"

	Date today.

	Time now.

"The result is selected, so you can erase it using the backspace key. Try it!"

SmalltalkImage current datedVersion.

"If you want a string and not just the tooltip, for example, if you want edit it or copy and paste the result of Print It in another tools simply double the command: 
 ''Ctrl + P''  ''Ctrl + P''

Try it on" 

	Date today.

	Time now

"Now you are a Print It Master!"

ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> reflection [
	^ Lesson
title: 'Reflection'
lesson:
'"You can inspect and change the system at runtime.

Take a look at method #ifFalse:ifTrue: source code of class True:"

(True>>#ifFalse:ifTrue:) sourceCode.

"Or just its comment:"

(True>>#ifFalse:ifTrue:) comment.

"Here''s all the methods I implement:"

ProfStef selectors.

"Now let''s create a new method to go to the next lesson:"

ProfStef class compile:''goToNextLesson
  self next''.

"Wow! I can''t wait to use my new method!"

ProfStef goToNextLesson.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> reflectionContinued [
	^ Lesson
title: 'Reflection continued'
lesson:
'"So cool, isn''t it? Before going further, let''s remove this method:"

ProfStef respondsTo: #goToNextLesson.

ProfStef class removeSelector: #goToNextLesson.

ProfStef respondsTo: #goToNextLesson.

"Pharo offers a lot of powerful reflective methods. 
Normally you should use when you build tools and not when you develop your application code.
So watch out.
Then move forward:"

ProfStef default executeMethod: (ProfStef lookupSelector: #next).'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> selecting [
	^ Lesson
title: 'Selecting text'
lesson:
'"You see that you are often selecting text. And Pharo offers some nice ways to do it super fast. 

Double click on the beginning of a line, select the line. 
So you do not have to move the mouse (and miss the last charater :), you can just double click and Print it. Much much faster isn''t

Try it on the following expressions:"

1000 factorial numberOfDigits

"It works also with space before"	
	
	1000 factorial numberOfDigits
	
"If you double click on the LAST character of a line, it selects the full line, Try it on the expressions above."	
	
"Cool! no?

Pharo can do better! If you have an expression on multiple lines you can double click at the end or the beginning of the expression to select the complete expression. Notice how you can select a line like the first one and get 3 printed or get the full result 24"

StPlaygroundPresenter new contents: ''1 * 2 
     * (4 + 8)''; open

"When you see class name in a text. You can jump to the class by Command + clicking on it,
Try it on"

OrderedCollection Bag Set 

"Yes there is no s at Collection so that you jump to the class :)"

"So now you are really a Do it Full Master!"
 


ProfStef next.'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> theEnd [
	^ Lesson
title: 'Tutorial done!'
lesson:
'"This tutorial is done. Enjoy programming with Pharo.

Don''t forget to read ''Pharo By Example'' found here:

	https://books.pharo.org/

You can run this tutorial again by evaluating:"

ProfStef go.

"Do you want to create your own interactive tutorial with ProfStef? That''s very easy!! How? There''s a ProfStef interactive tutorial for that :D
Just evaluate the following code:"

ProfStef goOn: HowToMakeYourOwnTutorial

"See you soon!"'
]

{ #category : 'tutorial' }
PharoSyntaxTutorial >> tutorial [
^ #(
welcome
doingVSPrinting
printing
inspecting
selecting
messageCanBeSeparatedByDot

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

understandYourself

reflection
reflectionContinued

pharoEnvironment

theEnd
)
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> understandYourself [
	^ Lesson
title: 'Understand yourself'
lesson:
'
"yourself is a simple message that does nothing but returning self (the receiver of the message). The message yourself deserves a bit of explanation.
Sending yourself is handy to get the receiver of a message. It is especially useful when using a cascade since all the cascaded messages are sent to the receiver. Let us look at an example.

Imagine that you have the following expression"
	
		#(''hello'' ''world'') copy at: 2 put: ''pharo''
	
"and that you would like to get the modified array #(''hello'' ''pharo'') copy - the receiver of the message at:put:. We use the message copy because the literal array is read only. 

Execute the expression above  returns ''pharo'' and not the receiver because the message at:put: always returns the second argument.

To get the receiver we can simply use a cascade as follows:"

		#(''hello'' ''world'') copy at: 2 put: ''pharo''; yourself

"and since yourself returns the receiver of the message we will get the modified array.

Stepping back using a cascade and yourself is equivalent to declare a variable, assign the expression and return the variable as follows:"


	| a |
	a := #(''hello'' ''world'').
	a at: 2 put: ''pharo''.
	a

"So you see the cascade is quite handy."





ProfStef next'
]

{ #category : 'lessons' }
PharoSyntaxTutorial >> welcome [
	^ Lesson
title: 'Welcome'
lesson:
'"Hello! I''m Professor Stef.

I''m here to  help you learn Pharo.
So let''s go to the first lesson. Select the text below, right-click and choose ''Do it''"

ProfStef next.'
]
