documentation
extensionExamples

	^ HelpTopic 
		title: 'Understanding Spotter extensions'
		contents:
'Spotter offers a contextual search mechanism. Every search step has an associated object that defines the context, and the concrete search specifications are defined as methods in the class of those objects.

The first instance that Spotter starts with by default is an instance of GTSpotter. For example, the logic of searching for classes throughout the image is defined as:

	GTSpotter>>#spotterForClassesFor: aStep
		<spotterOrder: 10>
		aStep listProcessor
				title: ''Classes'';
				allCandidates: [ Smalltalk allClassesAndTraits ];
				filter: GTFilterSubstring;
				itemIcon: #systemIcon;
				keyBinding: $b meta;
				wantsToDisplayOnEmptyQuery: false

Let''s look at this example:
- #title: specifies the label that appears as the search category title in the ui. This label can be used for filtering the search for only this category (see the "Category Filter Examples" section)
- #allCandidates: defines a block that returns the collection of elements that should be taken into account when searching. In this case, we take into account all classes and traits defined in the image.
- #filter: specifies the strategy to be used when trying to match each of the candidate. In this case, we are using a substring matching strategy to filter classes and traits according to an input query. Note that there is not specification of the string representation of the class that is actually taken into account. By default, the filter takes into account the result of sending #gtDisplayString to the candidate item, and in this case this information is precisely what we need. If we would want to specify another string, we could use:
	itemName: [ :eachItem | eachItem aMessageThatReturnsASpecificString ]
- #itemIcon: takes a block with one argument that returns the icon to be displayed in the result list for each matched item.
- #keybinding: specifies a keymap that when used, inserts the title of the category in the query box. In our case, typing
	GT   Cmd+b ==> #Classes GT
... essentially scoping the search only for this category.
- #wantsToDisplayOnEmptyQuery: specifies whether this processor should be active when thre is no input query. Essentially, if it is active when there is no input query, all items are being shown.

Now, imagine that you want to search methods inside a known class. To achieve this, you search for the class and then dive-in that class. Diving-in tells Spotter to change the context and offer processors for a class by looking at the Class class. For example, in Class, we find the following method that specifies how search for the methods inside a class should happen:

	TClass>>spotterMethodsFor: aStep
		<spotterOrder: 10>
		aStep listProcessor
				title: ''Instance methods'';
				allCandidates: [ self methods ];
				itemName: [ :method | method selector ];
				filter: GTFilterSubstring

To understand the option of specifying search processors in more details, take a look at the extentions present in the image and at the API of GTSpotterCandidatesListProcessor.'