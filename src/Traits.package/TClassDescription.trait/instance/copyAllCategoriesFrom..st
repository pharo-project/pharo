copying
copyAllCategoriesFrom: aClass 
	"Specify that the categories of messages for the receiver include all of 
	those found in the class, aClass. Install each of the messages found in 
	these categories into the method dictionary of the receiver, classified 
	under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]