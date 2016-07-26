opening
browse: aCollectionOfKeywords
	"open a SettingBrowser view which is able to browse all settings 
	that are declared with a pragma keyword which is at least
	one of the string contained in aCollectionOfKeywords
	example:
	(SettingBrowser browse: #('systemsettings'))
	browse all settings which make use of the pragma keyword 'systemsettings'
	
	Note that the default setting pragma keyword is 'systemsettings'
	"
	(self forKeywords: aCollectionOfKeywords) open