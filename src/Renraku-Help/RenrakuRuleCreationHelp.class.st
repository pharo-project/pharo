"
Each rule has a few fundamental features to implement in this book with will walk through all the steps of rule creation
"
Class {
	#name : #RenrakuRuleCreationHelp,
	#superclass : #RenrakuRuleHelp,
	#category : #'Renraku-Help'
}

{ #category : #pages }
RenrakuRuleCreationHelp class >> advancedChecking [

	^ HelpTopic
		title: 'Advanced Checking'
		contents: 
'While there is a default implementation which relies on #basicCheck: and creates an instance of ReTrivialCritique, it is advised to override the #check:forCritiquesDo: method. This method accepts an entity and a block which could be evaluated for each detected critique. This means that one rule can detect multiple critiques about one entity. For example if a rule checks for unused variables it can report all of them with a dedicated critique for each.

The block which should be evaluated for each critique may accept one argument: the critique object, this is why you have to evaluate it with #cull:. You may use the #critiqueFor: method if you don''t feel comfortable with critiques yes. For example:

    self critiqueFor: anEntity

will return ReTrivialCritique about the entity. Later you can update your code to create other kinds of critiques more suitable for your case.'
]

{ #category : #accessing }
RenrakuRuleCreationHelp class >> bookDescription [
	^ 'Each rule has a few fundamental features to implement in this book with will walk through all the steps of rule creation.
	
Make sure to RESET the CACHE after you add a rule. (last chapter)'
]

{ #category : #accessing }
RenrakuRuleCreationHelp class >> bookName [

	^ 'Creating Rules'
]

{ #category : #pages }
RenrakuRuleCreationHelp class >> checking [

	^ HelpTopic
		title: 'Checking'
		contents: 
'The easiest way to check an entity is to override the #basicCheck: method. The method accepts an entity as a parameter and returns true if there is a violation and false otherwise.'
]

{ #category : #pages }
RenrakuRuleCreationHelp class >> groupSeverity [

	^ HelpTopic
		title: 'Group and Severity'
		contents: 
'It''s a good idea to assign your rule to a specific group. For this override the #group method and return string with the name of the group. While you can use any name you want, maybe you would like to put your rule into one of the existing groups: ',
(ReRuleManager availableRuleGroups joinUsing: ', '), '.

You can also specify the severity of your rue by returning one of: #information, #warning, or #error symbols from the #severity method.'
]

{ #category : #pages }
RenrakuRuleCreationHelp class >> interestInEntities [

	^ HelpTopic
		title: 'Specifying an Interest in Entities'
		contents: 
'The class-side methods #checksMethod, #checksClass, #checksPackage and #checksNode return true is the rule checks methods, classes or traits, packages and AST nodes respectively. Tools will pass entities of the specified type to the rule for checking. A rule may check multiple types of entities but please avoid checks for types inside of rules. E.g. if a rule checks whether an entity is named with a swearing word and does this by obtaining the name of the entity and matching substrings. It is completely fine to specify that the rule checks classes and packages as you don''t have to distinguish which entity was passed to you.'
]

{ #category : #pages }
RenrakuRuleCreationHelp class >> nameRationale [

	^ HelpTopic
		title: 'Name and Rationale'
		contents: 
'Each rule should provide a short name string returned from the #name method. You also have to override the #rationale method to return a detailed description about the rule. You may also put the rationale in the class comment, as by default #rationale method returns the comment of the rule''s class.'
]

{ #category : #accessing }
RenrakuRuleCreationHelp class >> pages [
	^ #(subclass nameRationale interestInEntities checking advancedChecking testing groupSeverity resetCache)
]

{ #category : #pages }
RenrakuRuleCreationHelp class >> resetCache [

	^ HelpTopic
		title: '! Reset Cache'
		contents: 
'To have quality assistant (and maybe other tools) pick up your changes you have to reset the cache. Do this by going to

System > Settings > Code Browsing > QualityAssistant > Renraku > Rule Cache

and pressing the reset button. Or simply executing

ReRuleManager reset


When you load complete rules into the system, the cache will be reset automatically. But as you are creating a new rule and it is in the incomplete state you have to reset the cache once you are ready.'
]

{ #category : #pages }
RenrakuRuleCreationHelp class >> subclass [

	^ HelpTopic
		title: 'Subclass'
		contents: 
'Subclass ReAbstractRule or one of the special rule classes that will be discussed. Most of tools rely on the classes hierarchy to select the rules for checking.'
]

{ #category : #pages }
RenrakuRuleCreationHelp class >> testing [

	^ HelpTopic
		title: 'Testing'
		contents: 
'It is fairly easy to run your rule and obtain the results. Just create an instance of it an send it the #check: message with the entity you want to check. The result is a collection of critiques. For example inspecting

	RBExcessiveMethodsRule new check: Object

should give you a collection with one critique (because the Object class always has many methods ;) ). Go on click on the critique item and inspect it. You will see that there is a special "description" tab. This is the power of critique objects, they can present themselves in a different way. Guess what: you can even visualize the critique if needed.'
	
]
