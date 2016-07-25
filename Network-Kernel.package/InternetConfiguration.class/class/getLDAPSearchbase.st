lookups
getLDAPSearchbase
	"Return the LDAP thing"
	"InternetConfiguration getLDAPSearchbase"

	^self primitiveGetStringKeyedBy: 'LDAPSearchbase'
