A SettingDeclaration is an handler for a writtable setting value. 
Its main responsibility is allow real value changing and to hold descriptive data about the setting. When the real value is changed, all declared listeners are notified. The standard event mechanism is used for that purpose. See #value: method which is call in order to change a setting value. Whatever object can declare its interest in the real value changes by sending #whenChangedSend:to: to a setting. See notification protocol.

For convenience, my olds values are registered in a list. This list is used by the SystemSettingBrowser in order to easily retrieve previous values.

The type instance variable is a Symbol. It is used by the SystemSettingBrowser in order to build the input widget of a setting.
It can be the name of the class or the name of a superclass of the value. In that case, the input widget is built by #settingInputWidgetForNode: implemented by the class which name is the value of the type instance variable. As an example, the input widget for a Boolean is built by Boolean class>>settingInputWidgetForNode:.

Type value is not limited to be a class name. It can also be whatever symbol. In that case, the selector corresponding to the message to send to the setting in order to build the inputWidget is built by SettingDeclaration>>localInputWidgetSelector. If the type instance variable is not set, then it is dynamically set to the class name of the value. 

See SettingManager comment for more explanations.

Instance Variables
	default:		<Object>
	getSelector:		<Object>
	ghostHelp:		<Object>
	setSelector:		<Object>
	type:		<Object>

default
	- xxxxx

getSelector
	- xxxxx

ghostHelp
	- xxxxx

setSelector
	- xxxxx

type
	- xxxxx
