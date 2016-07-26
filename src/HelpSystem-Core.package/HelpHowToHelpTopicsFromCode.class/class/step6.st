pages
step6
	^HelpTopic
		title: 'Step 6 - Add more structure'
		contents:
	'STEP 6 - ADD MORE STRUCTURE

If you add a new subclass to your custom help class and repeating step 2 to 4 you can
profide new substructures (subbooks) since the help books are mapped to the 
class hierarchy. Example:
 
MyAppHelp subclass: #MyAppTutorial
    	instanceVariableNames: ''''
	  classVariableNames: ''''
	  poolDictionaries: ''''
	  category: ''MyApp-Help'' 

then implement a #bookName, the pages and a #pages method as before on this new 
class and reopen the help browser. 		
'
