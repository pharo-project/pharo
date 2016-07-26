<help>

! The User interface
A SettingBrowser allows the browsing as well as the editing of setting values.
For each setting, a label and an input widget allowing the change of the setting value are shown.

!! Browsing standards settings
In order to open a setting browser one can also use the expression below:
--------------------
SettingBrowser open
--------------------
It opens a SettingBrowser for all settings declared with the standard 'systemsettings' pragma keyword. 
To see how these settings are declared in the image, you can browse all senders of #systemsettings:
--------------
SystemNavigation new  browseAllSendersOf: #systemsettings
--------------

!! Browsing application specific settings
For specific applications, other pragma keywords can be used. These keywords can be passed as follow:
--------------------
(SettingBrowser forKeywords: #('blob')) open.
--------------------
Then, only settings which are declared with the keyword 'blob' are viewed. 
Here is an example of such a setting declared in the class side of a class BlobSettings 
--------------------
BlobSettings class>>blobSettingOn: aBuilder
	<blob>
	(aBuilder group: #blobEditing) 
		label: 'Editing' translated;
		parent: #blobBrowsing; 
		description: 'All settings concerned with blob editing' translated;
		with: [	
			(aBuilder setting: #color)	...
--------------------
The expression 'SettingBrowser open' is then equivalent to the '(SettingBrowser forKeywords: #('systemsettings')) open' expression.

!! Filtering
The SettingBrowser has a filtering functionality to limitate the number of settings. You can enter a token in the input field of the toolbar in order to show only settings that are matching the token.
Two filtering modes are allowed:  with a substring or with a regular expression.
- with a substring: only settings which name, description, pragma class or method selector includes this substring are viewed; 
- with a regular expression: the same as with a substring except that the input is used as a regular expression. This filtering is applied if the Regexp checkbox is checked.

! How to declare a setting
A setting is declared with a method class side. This kind of method takes a builder as argument and a standard setting is tagged with the <systemsettings> pragma. The builder argument serves as a facade for the declaration.

!!A simple boolean setting example

Let's start with a setting declaration example:
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		setSelector: #caseSensitiveFinds:;
		getSelector: #caseSensitiveFinds;
		target: TextEditor;
		parent: #codeEditing.

For this setting to be declared, we make the asumption that we have TextEditor class>>caseSensitiveFinds and TextEditor class>>caseSensitiveFinds: methods in order to change the preference value. 
To declare a setting, just send #setting: to the builder with its identifier, a Symbol passed as argument. It creates a setting node. Then you can set the label, the description with #label: and #description sent to the newly created setting node. You also have to set the selectors for setting and getting the preference value as well as the target to which these accessors are sent  (often a class). This is done by sending respectively, #setSelector:, #getSelector: and #target: to the setting node.
Because all settings are organized in trees we need a way to indicate what is the position of the setting node in the overall setting trees list. In fact it can be done two ways. The first way is to use the #parent: message (A second possibility is to declare a subtree in one method, it is explained later in this documentation).The #parent: message is send for non root settings. #parent takes the identifier of the parent setting as argument.

You may notice that in this example,  if we don't take into account the $: at the end of the setting accessor, the getting and the setting accessors are the same. This is often the case. You can simply set the setter and the getter by sending the #selector: message to the setting node. Thus the declaration is simplified as follow:
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		selector: #caseSensitiveFinds;
		target: TextEditor;
		parent: #codeEditing.

You may also notice that the identifier of the setting is then the same as the selector. In this case, you can omit to set the selector because by default, the identifier is used as the selector for getting the preference value and the identifier concatenated with a $: at the end is used as the setting selector. Thus the declaration is simplified again as follow:
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		target: TextEditor;
		parent: #codeEditing.

Finally, if you decide to declare a setting directly in the class which implements the selector, the target information can be also omitted because the class in which the setting is declared is used by default. Thus the simplest declaration can be:
TextEditor class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		parent: #codeEditing.

!!Declaring a subtree in one single method
Directly declaring a sub-tree of settings in one method is also possible. Then, typically, a root group is declared for the application settings and the children settings themselves are also declared within the same method as in the example below in which #formatCommentWithStatements and #indentString are directly declared as children of the #configurableFormatter setting:

RBConfigurableFormatter class>>settingsOn: aBuilder
	<systemsettings>	
	(aBuilder group: #configurableFormatter)
		target: self;
		parent: #refactoring;
		label: 'Configurable Formatter' translated;
		description: 'Settings related to the formatter' translated;
		with: [
			(aBuilder setting: #formatCommentWithStatements)
				label: 'Format comment with statements' translated.
			(aBuilder setting: #indentString)
				label: 'Indent string' translated]

!!Optional setting
Any setting can have children. In the case where a boolean setting is used as a parent, then, its children are chown only if the parent preference value is true.

!!Range setting
You send the #range: message to the builder instead of the #setting: message. In addition, you send the #range: message to the setting with an interval as argument in order to indicate the valid range.
screenMarginSettingOn: aBuilder
	<systemsettings>
	(aBuilder range: #fullscreenMargin)
		target: SystemWindow;
		parent: #windows;
		label: 'Fullscreen margin' translated;
		description: 'Specify the amount of space that is let around a windows when it''s opened fullscreen' translated;
		range: (-5 to: 100).

!!List setting
For this kind of setting, the SettingBrowser will show a drop list. Here is an example for the window position strategy. Notice that the setting is declared by sending the #pickOne message to the builder. Notice also that the list of valid values are given by sending the #domainValues: message to the setting. The argument is an array of association. Each association key is the label that is shown and the corresponding association value gives the value that will be assigned to the preference.
windowPositionStrategySettingsOn: aBuilder
	<systemsettings>
	(aBuilder pickOne: #usedStrategy) 
		label: 'Window position strategy' translated;
		target: RealEstateAgent;
		domainValues: {
			'Reverse Stagger' translated -> #staggerFor:initialExtent:world:. 
			'Cascade' translated -> #cascadeFor:initialExtent:world:. 
			'Standard' translated -> #standardFor:initialExtent:world:};

!!Launcher
A launcher is a particular setting. It allows to launch a script directly from the setting browser. Imagine that you have changed some settings and that you need to evaluate a script in order to update some other objets. It can be used also to configurate globally a package of the entire image.
As an example, in order to use True Type Fonts, the system must be updated by collecting all the available TT fonts. This can be done by evaluating the following expression:
-------------
FreeTypeFontProvider current updateFromSystem
-------------
In order to be able to launch this script from the setting browser, you have to declare a launcher. For example, look-at how the script for the TT fonts is declared in GraphicFontSettings class >> #standardFontsSettingsOn:.

GraphicFontSettings class >> #standardFontsSettingsOn:
	<systemsettings>
	(aBuilder group: #standardFonts)
		...
		(aBuilder launcher: #updateFromSystem)
				order: 1; 
				target: FreeTypeFontProvider;
				targetSelector: #current;
				description: 'Update available fonts by scanning the current system';
				script: #updateFromSystem;
				label: 'Update fonts from system' translated.

Notice that you send #launcher: to the builder in order to create the setting node, then you send #script: to the created node with the selector of the script passed as argument.
</help>

Implementation details
See also SettingNode, SettingTree, SettingNodeBuilder and SettingTreeBuilder classes.

Instance Variables
	roots:		<Collection of SettingTreeNode>
	searchedText:		<String>
	status:		<WriteStream>
	collector:		<SettingCollector>

roots
	- the roots of the viewed setting trees

searchedText
	- the string used as a filter token or regexp in order to select viewed settings

status
	- a WriteStream used in order to store useful information which are shown when no current item is selected

collector
	- The SettingCollector which has the responsibility to collect settings and to build viewed trees
