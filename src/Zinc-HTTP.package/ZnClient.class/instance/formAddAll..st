accessing request
formAddAll: keyedCollection
	"Add all key->value associations of keyedCollection to 
	the application/x-www-form-urlencoded entity of the current request."

	self applicationFormUrlEncodedEntity 
		addAll: keyedCollection 