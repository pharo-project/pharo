"
There are certain special rules with predefined functionality that allows to easily perform complex checks. The subsections of this Book contain references to the rules
"
Class {
	#name : #RenrakuSpecialRulesHelp,
	#superclass : #RenrakuRuleCreationHelp,
	#category : #Renraku-Help
}

{ #category : #accessing }
RenrakuSpecialRulesHelp class >> bookDescription [
	^ 'There are certain special rules with predefined functionality that allows to easily perform complex checks. The subsections of this Book contain references to the rules'
]

{ #category : #accessing }
RenrakuSpecialRulesHelp class >> bookName [

	^ 'Special Rules'
]

{ #category : #pages }
RenrakuSpecialRulesHelp class >> matchRules [

	^ self topicForClass: ReNodeMatchRule
]

{ #category : #accessing }
RenrakuSpecialRulesHelp class >> pages [
	^ #(matchRules rewriteRules)
]

{ #category : #pages }
RenrakuSpecialRulesHelp class >> rewriteRules [

	^ self topicForClass: ReNodeRewriteRule
]
