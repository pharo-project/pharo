A PragmaCollector is used in order to collect some Pragma instances. A PragmaCollector makes use of SystemChangeNotifier event notifications in order to maintain its contents up-to-date according to its filter: when a method is added, removed or updated, if the method is defined with a pragma which is acceptable according to its filter, then the collector contents is updated. A PragmaCollector makes use of an announcer in order to notify all registered listeners when a pragma is added, removed or updated. A PragmaAnnouncement is announced when a Pragma is added, removed or updated. Corresponding announcement classes are, respectiveley, PragmaAdded, PragmaRemoved and PragmaUpdated. 

Explore the result of the expression below. In the collected instance variable should be stored all pragmas of the system:
---------------------------
(PragmaCollector filter: [:pragma | true]) reset
---------------------------

In the following example, collected pragma are thoses with the 'primitive:' keyword (<primitive:>)
---------------------------
(PragmaCollector filter: [:prg | prg keyword = 'primitive:']) reset
---------------------------

Instance Variables	
	announcer:		<Announcer>	
	collected:		<Collection>
	filter:			<Block or MessageSend>
				
announcer
	the announcer which is used to announce the adding, the removing or the updating of a method with an acceptable pragma declaration

collected		
	the current collection of Pragma
	
filter
	a block or a message send which is used in order to filter the pragma. This is a one argument valuable. When evaluated, the candidate pragam is passed as argument and the result must be a boolean. 
	