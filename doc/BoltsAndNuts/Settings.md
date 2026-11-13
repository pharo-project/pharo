# Settings

## The settings browser

The settings browser allows the browsing as well as the editing of setting values.
For each setting, a label and an input widget allowing the change of the setting value are shown.

It is opened from the world menu (Pharo > Settings) or with the expression below:

```st
(StSettingsBrowser newApplication: StPharoApplication current) open
```

It shows all settings declared with the standard `systemsettings` pragma keyword.
To see how these settings are declared in the image, you can browse all senders of #systemsettings:

```st
SystemNavigation new browseAllSendersOf: #systemsettings
```

The search field of the browser limits the shown settings to the ones whose label matches the entered text.

The Load and Export buttons load and store the setting values from and to the system preferences file (see "Storing and loading settings" below).

## How to declare a setting

A setting is declared with a class-side method. This kind of method takes a builder as argument and a standard setting is tagged with the `<systemsettings>` pragma. The builder argument serves as a facade for the declaration.

### A simple boolean setting example

Let's start with a setting declaration example:

```st
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds)
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		setSelector: #caseSensitiveFinds:;
		getSelector: #caseSensitiveFinds;
		target: TextEditor;
		defaultValue: false;
		parentName: #codeEditing.
```

For this setting to be declared, we make the assumption that we have `TextEditor class>>caseSensitiveFinds` and `TextEditor class>>caseSensitiveFinds:` methods to change the preference value.

To declare a setting, just send `setting:` to the builder with its name, a Symbol passed as argument. It creates a setting. Then you can set the label and the description with `label:` and `description:`. You also have to set the selectors for setting and getting the preference value as well as the target to which these accessors are sent (often a class). This is done by sending respectively `setSelector:`, `getSelector:` and `target:` to the setting. `defaultValue:` gives the value of the setting when it is not changed: setting values equal to their default value are not stored.

Because all settings are organized in trees, we need a way to indicate the position of a setting in the tree. It can be done two ways. The first way is to send `parentName:` with the name of the parent group as argument. A second possibility is to declare a subtree in one method, as explained later in this documentation. Since settings find their parent by name, group names must be unique.

You may notice that in this example, if we don't take into account the $: at the end of the setting accessor, the getting and the setting accessors are the same. This is often the case. You can simply set the setter and the getter by sending the `selector:` message to the setting. Thus the declaration is simplified as follow:

```st
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds)
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		selector: #caseSensitiveFinds;
		target: TextEditor;
		defaultValue: false;
		parentName: #codeEditing.
```

You may also notice that the name of the setting is then the same as the selector. In this case, you can omit to set the selector because by default, the name is used as the selector for getting the preference value and the name concatenated with a $: at the end is used as the setting selector. Thus the declaration is simplified again as follow:

```st
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds)
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		target: TextEditor;
		defaultValue: false;
		parentName: #codeEditing.
```

Finally, if you decide to declare a setting directly in the class which implements the selector, the target information can be also omitted because the class in which the setting is declared is used by default. Thus the simplest declaration can be:

```st
TextEditor class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds)
		label: 'Case sensitive search' translated;
		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion' translated;
		defaultValue: false;
		parentName: #codeEditing.
```

When the value is not reachable through accessors, blocks can be given instead with `readBlock:` and `writeBlock:`:

```st
	(aBuilder setting: #caseSensitiveFinds)
		readBlock: [ TextEditor caseSensitiveFinds ];
		writeBlock: [ :aBoolean | TextEditor caseSensitiveFinds: aBoolean ];
		...
```

### Declaring a subtree in one single method

Directly declaring a subtree of settings in one method is also possible. Then, typically, a root group is declared for the application settings and the children settings themselves are also declared within the same method as in the example below in which `formatCommentWithStatements` and `indentString` are directly declared as children of the #configurableFormatter group:

```st
RBConfigurableFormatter class>>settingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #configurableFormatter)
		target: self;
		parentName: #refactoring;
		label: 'Configurable Formatter' translated;
		description: 'Settings related to the formatter' translated;
		with: [
			(aBuilder setting: #formatCommentWithStatements)
				label: 'Format comment with statements' translated.
			(aBuilder setting: #indentString)
				label: 'Indent string' translated ]
```

The children use the target of their group when they do not have their own target.

### Activable group

A group can also be switched on and off, for example a feature that has its own settings. Such a group is shown with a checkbox. It is declared by sending `isActive:` to the group, with blocks to read and write its state:

```st
SHPreferences class>>settingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #'Syntax Highlighting')
		parentName: #codeBrowsing;
		isActive: true;
		readBlock: [ self enabled ];
		writeBlock: [ :active | self enabled: active ];
		with: [ ... ]
```

Without blocks, the state is read and written with the group name as selector, sent to the group target.

### Range setting

You send the `range:` message to the builder instead of the `setting:` message. In addition, you send the `range:` message to the setting with an interval as argument in order to indicate the valid range. The current value must be one of the interval values.

```st
screenMarginSettingOn: aBuilder
	<systemsettings>
	(aBuilder range: #fullscreenMargin)
		target: SystemWindow;
		parentName: #windows;
		label: 'Fullscreen margin' translated;
		description: 'Specify the amount of space that is let around a windows when it''s opened fullscreen' translated;
		range: (-5 to: 100).
```

### List setting

For this kind of setting, the settings browser shows a drop list. Here is an example for the window position strategy. Notice that the setting is declared by sending the `pickOne:` message to the builder. Notice also that the list of valid values are given by sending the `domainValues:` message to the setting. The argument is an array of association. Each association key is the label that is shown and the corresponding association value gives the value that will be assigned to the preference. Values that are not associations are shown with their `asString`.

```st
windowPositionStrategySettingsOn: aBuilder
	<systemsettings>
	(aBuilder pickOne: #usedStrategy)
		label: 'Window position strategy' translated;
		target: RealEstateAgent;
		domainValues: {
			'Reverse Stagger' translated -> #staggerFor:initialExtent:world:.
			'Cascade' translated -> #cascadeFor:initialExtent:world:.
			'Standard' translated -> #standardFor:initialExtent:world: }
```

### Button

A button is a particular setting. It allows one to run an action directly from the settings browser. Imagine that you have changed some settings and that you need to evaluate a script in order to update some other objects.

A button is declared by sending `button:` to the builder. When the button is clicked, its name is sent as a message to its target. Then the settings browser refreshes the page, so that it shows the settings changed by the action:

```st
GraphicFontSettings class>>standardFontsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #standardFonts)
		...
		with: [
			(aBuilder button: #pickFont)
				target: self;
				label: 'Pick font for all';
				buttonLabel: 'Pick'.
			... ]

GraphicFontSettings class>>pickFont
	FontChooser
		openModalWithTitle: 'Font for all font'
		initialFont: StandardFonts defaultFont
		onAcceptDo: [ :aFont | self resetAllFontsTo: aFont ]
```

### Transient setting

A setting declared with `isTransient: true` is never stored.

## Storing and loading settings

Settings are built, stored and loaded by a `SettingsManager`. The system settings are stored in the `system-settings.ston` file of the preferences folder of the current Pharo version, and loaded at startup:

```st
SettingsManager forSystemSettings storeSettings.
SettingsManager forSystemSettings loadSettings.
```

For specific applications, other pragma keywords can be used to declare their own settings, which can be stored in their own file:

```st
BlobSettings class>>blobSettingOn: aBuilder
	<blob>
	(aBuilder group: #blobEditing)
		label: 'Editing' translated;
		description: 'All settings concerned with blob editing' translated;
		with: [
			(aBuilder setting: #color) ... ]
```

```st
(SettingsManager forPragmaNamed: #blob)
	settingsFile: FileLocator preferences / 'blob-settings.ston';
	storeSettings
```
