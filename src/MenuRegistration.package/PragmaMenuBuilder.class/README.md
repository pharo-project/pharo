For the impatients, see MenuRegistrationExample class methods and try it with:
---------------
((PragmaMenuBuilder pragmaKeyword: MenuRegistrationExample pragmaKeyword model: nil) menuEntitled: 'World') popUpInWorld
---------------

PragmaMenuBuilder is for the dynamic building of menus based on pragmas. A PragmaMenuBuilder instance is the root of a tree of MenuRegistration instances.
The basic principle is that each menu sub-tree is specified by a method which is tagged by a specific pragma. Such methods are dynamically retrieved and then evaluated with a MenuRegistration passed as argument (see #retrieveRegistrations). The result is a tree of MenuRegistration which roots are stored in my itemList inst. var.
After the tree of MenuRegistration has been built, it is re-organized (re-organization is based on the parent declaration) and is re-ordered (based on the MenuRegistration order indications). Then the tree of MenuRegistration can serve as input for the building of a PluggableMenuSpec. The PluggableMenuSpec is itself used in order to build a MenuMorph with the help of the current ToolBuilder. (see MenuRegistration comment for more informations about how to specify menu entries).

The tree of MenuRegistration is built by #buildTree in three steps (1) the  collecting of the MenuRegistration instances (2) the re-organization and (3) the sorting:

1) The first step consists in evaluating all pragma methods by passing a builder (a PragmaMenuBuilder instance) as argument. Each pragma method invocation build a sub-tree which root is added to the builder itemList collection. (see #collectRegistrations).
As an example, this first step could produce a tree as follow (stored in a PragmaMenuBuilder itemList inst var) :
				#Tools						#'Other tool', parentName: #Tools
				/	\										|
	(#Worspace)	(#browser)						(#'Test runner' )

2) The second step consists is re-organizing the tree. A MenuRegistration can be declared with a particular parent name (by sending #parent: to it with a symbol as argument). If the parentName of a MenuRegistration X is the name of another MenuRegistration Z, then it means that X must be placed as a child of Z. This is the goal of this re-arrangement step which moves badly placed nodes at their good place. (see #arrangeRegistrations).
With previous example, the second step produces:
						#Tools								
				/		|			\
	(#Worspace)	(#browser)		#'Other tool' , parentName: #Tools
											|
									(#'Test runner')

2) The third step consists in sorting the tree according to the order inst. var. value of each MenuRegistration. This is done in two passes: the first pass tries to assign as much order inst. var. as possible (If an item is given with a specific order, then, previous and following items order can be automatically computed - see #assignOrderWithBlock: and #orderAssignBlock). The second pass consists in a smple sort according to a sort block given by #itemSortBlock.


Instance Variables
	model:		<Object>
	pragmaCollector:		<PragmaCollection>
	pragmaKeywords:		<Collection of Symbol>
	currentRoot: 			<MenuRegistration>

model
	- Serves as the default target for the menu. Note that a default target can also be declared at menu item level

pragmaKeywords
	- The list of pragma keywords used for the declaring of my menu items

pragmaCollector
	- The PragmaCollector associated with this builder. When a method declared with the same pragma as my pragmaKeyword is updated/added/removed my menu items are recomputed so that the resulting menu is always in sync with currently declared items.
	
currentRoot
	- the current MenuRegistration in which new items are to be added

