A CheckListSettingDeclaration is a setting for which the value domain is a list. Each element of the list is an instance of FixedSettingValue. If domainValues is set, then the list of valid values is constant (initialized at declaration time). Instead, if getter is set, then the setting list is always dynamically computed. See SettingManager comment for more explanations. 

Instance Variables
