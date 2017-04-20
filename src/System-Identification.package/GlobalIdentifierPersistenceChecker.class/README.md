I check whether a persistence should be stored. I return answer of `FileIdentifier home exists`. Such use case is important for a server deployment, where HOME directory may not exists. See https://pharo.fogbugz.com/f/cases/19944

I collaborate with GlobalIdentifierPersistence that uses me for checking before any disk operation.