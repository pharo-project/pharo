A HelpTopic provides content information that can be used as a help to the user.
It can be labeled with a title, identified using an (optional) unique key and marked 
with an (optional) icon.

Help topics form a hierarchy since any topic is able to have zero or more
subtopics. 


Instance Variables
	contents:		<Object>      The help topic contents
	icon:			<Form|nil>	   An optional icon for the topic
	key:			<String|nil>    An optional unique key
	subtopics:	      <Collection>  A collection of subtopics
	title:			<String>        The title

contents
	- The help topic contents - typically containing the help topics information

icon
	- An optional icon for the topic

key
	- An optional unique key which can be used to identify the topic. 

subtopics
	- A collection of subtopics. 
	  By default the subtopics are not sorted, so the insertion order is used. 
	  If necessary it is possible to sort the subtopics by title.

title
	- A meaninful title for the help topic
