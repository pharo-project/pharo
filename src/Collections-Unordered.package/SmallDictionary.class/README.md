Description
--------------------------

SmallDictionary is a special dictionary optimized for small collections. In addition to the normal dictionary protocol, it also supports an #empty message which "empties" the collection but may hang on to the original elements (so it could collect garbage). Without #empty we would either need to create a new dictionary or explicitly remove everything from the dictionary. Both of these take more time and #empty.

Be careful, I cannot have *nil* as key. 

Public API and Key Messages
--------------------------

- #at: aKey put: aValue / #at: aKey ifAbsentPut: aValue 		allow to add an element.
  
- #at: aKey / #at: aKey ifAbsent: aBlock / #at: aKey ifPresent: aBlock ifAbsent: aBlock 		allow to access my values.

- #keysDo: aBlock / #valuesDo: aBlock / #associationsDo: 		allow to iterate on me effectively

Examples 
--------------------------

To create a dictiony with indexes as key: 

	SmallDictionary withAll: #(7 3 1 3)   		"returns:  a SmallDictionaryDictionary(1->7 2->3 3->1 4->3 "

To use Objects as key (here symbols): 

	colors := SmallDictionary new 
				at: #yellow put: Color yellow; 
				at: #blue put: Color blue;
				at: #red put: Color red;
				yourself.
				
	colors at: #yellow. 	"returns:  Color yellow"
	colors keys          		"returns: a Set(#blue #yellow #red)"
	colors values     		"returns:  {Color blue. Color yellow. Color red}"

	colors empty 	"a SmallDictionary()"

Internal Representation and Key Implementation Points.
--------------------------

    Instance Variables
	keys:		<Array>		Array of keys (we don't use Associations for our key value pairs)
	size:			<Integer>	Size of the dictionary
	values:		<Array>		Array of our values


    Implementation Points