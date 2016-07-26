lookups
getLDAPServer
	"Return the LDAP server"
	"InternetConfiguration getLDAPServer"

	^self primitiveGetStringKeyedBy: 'LDAPServer'
