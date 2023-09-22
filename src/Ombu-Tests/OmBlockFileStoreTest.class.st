Class {
	#name : 'OmBlockFileStoreTest',
	#superclass : 'OmFileStoreTest',
	#instVars : [
		'targetReference',
		'bigStore'
	],
	#category : 'Ombu-Tests',
	#package : 'Ombu-Tests'
}

{ #category : 'accessing' }
OmBlockFileStoreTest class >> multiBlockOmbu [
	"The second comment in this method is used to generate test data for #testEntryForIfPresentIfAbsent"
"OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.404694+01:00' ],
		#prior : OmReference [ 'Pharo10-SNAPSHOT-64bit-14f5413.8z1nmd2yzd4bxueyxpz9rzxql', '5' ],
		#self : OmReference [ '1' ]
	},
	#content : EpMethodAddition {
		#method : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 1',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.498412+01:00' ],
		#prior : OmReference [ '1' ],
		#self : OmReference [ '2' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 1',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 2',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.540124+01:00' ],
		#prior : OmReference [ '2' ],
		#self : OmReference [ '3' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 2',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 3',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.574571+01:00' ],
		#prior : OmReference [ '3' ],
		#self : OmReference [ '4' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 3',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 4',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.597862+01:00' ],
		#prior : OmReference [ '4' ],
		#self : OmReference [ '5' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 4',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 5',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.625611+01:00' ],
		#prior : OmReference [ '5' ],
		#self : OmReference [ '6' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 5',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 6',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.653653+01:00' ],
		#prior : OmReference [ '6' ],
		#self : OmReference [ '7' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 6',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 7',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.676314+01:00' ],
		#prior : OmReference [ '7' ],
		#self : OmReference [ '8' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 7',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 8',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.703659+01:00' ],
		#prior : OmReference [ '8' ],
		#self : OmReference [ '9' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 8',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 9',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.730653+01:00' ],
		#prior : OmReference [ '9' ],
		#self : OmReference [ '10' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 9',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 10',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.754558+01:00' ],
		#prior : OmReference [ '10' ],
		#self : OmReference [ '11' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 10',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 11',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.781475+01:00' ],
		#prior : OmReference [ '11' ],
		#self : OmReference [ '12' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 11',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 12',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.804535+01:00' ],
		#prior : OmReference [ '12' ],
		#self : OmReference [ '13' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 12',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 13',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.833737+01:00' ],
		#prior : OmReference [ '13' ],
		#self : OmReference [ '14' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 13',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 14',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.860597+01:00' ],
		#prior : OmReference [ '14' ],
		#self : OmReference [ '15' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 14',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 15',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.888144+01:00' ],
		#prior : OmReference [ '15' ],
		#self : OmReference [ '16' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 15',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 16',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.915719+01:00' ],
		#prior : OmReference [ '16' ],
		#self : OmReference [ '17' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 16',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 17',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.944462+01:00' ],
		#prior : OmReference [ '17' ],
		#self : OmReference [ '18' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 17',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 18',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.967302+01:00' ],
		#prior : OmReference [ '18' ],
		#self : OmReference [ '19' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 18',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 19',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:40.989824+01:00' ],
		#prior : OmReference [ '19' ],
		#self : OmReference [ '20' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 19',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 20',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.017284+01:00' ],
		#prior : OmReference [ '20' ],
		#self : OmReference [ '21' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 20',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 21',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.043131+01:00' ],
		#prior : OmReference [ '21' ],
		#self : OmReference [ '22' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 21',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 22',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.067873+01:00' ],
		#prior : OmReference [ '22' ],
		#self : OmReference [ '23' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 22',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 23',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.090382+01:00' ],
		#prior : OmReference [ '23' ],
		#self : OmReference [ '24' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 23',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 24',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.117545+01:00' ],
		#prior : OmReference [ '24' ],
		#self : OmReference [ '25' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 24',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 25',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.148951+01:00' ],
		#prior : OmReference [ '25' ],
		#self : OmReference [ '26' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 25',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 26',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.179442+01:00' ],
		#prior : OmReference [ '26' ],
		#self : OmReference [ '27' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 26',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 27',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.203359+01:00' ],
		#prior : OmReference [ '27' ],
		#self : OmReference [ '28' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 27',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 28',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.232362+01:00' ],
		#prior : OmReference [ '28' ],
		#self : OmReference [ '29' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 28',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 29',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.25843+01:00' ],
		#prior : OmReference [ '29' ],
		#self : OmReference [ '30' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 29',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 30',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.288812+01:00' ],
		#prior : OmReference [ '30' ],
		#self : OmReference [ '31' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 30',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 31',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.31742+01:00' ],
		#prior : OmReference [ '31' ],
		#self : OmReference [ '32' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 31',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 32',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.344849+01:00' ],
		#prior : OmReference [ '32' ],
		#self : OmReference [ '33' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 32',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 33',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.368584+01:00' ],
		#prior : OmReference [ '33' ],
		#self : OmReference [ '34' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 33',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 34',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.396468+01:00' ],
		#prior : OmReference [ '34' ],
		#self : OmReference [ '35' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 34',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 35',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.424756+01:00' ],
		#prior : OmReference [ '35' ],
		#self : OmReference [ '36' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 35',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 36',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.447721+01:00' ],
		#prior : OmReference [ '36' ],
		#self : OmReference [ '37' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 36',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 37',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.475632+01:00' ],
		#prior : OmReference [ '37' ],
		#self : OmReference [ '38' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 37',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 38',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.50351+01:00' ],
		#prior : OmReference [ '38' ],
		#self : OmReference [ '39' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 38',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 39',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.531493+01:00' ],
		#prior : OmReference [ '39' ],
		#self : OmReference [ '40' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 39',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 40',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.562184+01:00' ],
		#prior : OmReference [ '40' ],
		#self : OmReference [ '41' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 40',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 41',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.590065+01:00' ],
		#prior : OmReference [ '41' ],
		#self : OmReference [ '42' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 41',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 42',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.617732+01:00' ],
		#prior : OmReference [ '42' ],
		#self : OmReference [ '43' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 42',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 43',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.64081+01:00' ],
		#prior : OmReference [ '43' ],
		#self : OmReference [ '44' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 43',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 44',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.66605+01:00' ],
		#prior : OmReference [ '44' ],
		#self : OmReference [ '45' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 44',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 45',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.690893+01:00' ],
		#prior : OmReference [ '45' ],
		#self : OmReference [ '46' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 45',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 46',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.718834+01:00' ],
		#prior : OmReference [ '46' ],
		#self : OmReference [ '47' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 46',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 47',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.742018+01:00' ],
		#prior : OmReference [ '47' ],
		#self : OmReference [ '48' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 47',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 48',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.764125+01:00' ],
		#prior : OmReference [ '48' ],
		#self : OmReference [ '49' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 48',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 49',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.786774+01:00' ],
		#prior : OmReference [ '49' ],
		#self : OmReference [ '50' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 49',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 50',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.809775+01:00' ],
		#prior : OmReference [ '50' ],
		#self : OmReference [ '51' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 50',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 51',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.832102+01:00' ],
		#prior : OmReference [ '51' ],
		#self : OmReference [ '52' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 51',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 52',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.860061+01:00' ],
		#prior : OmReference [ '52' ],
		#self : OmReference [ '53' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 52',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 53',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.882387+01:00' ],
		#prior : OmReference [ '53' ],
		#self : OmReference [ '54' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 53',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 54',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.910991+01:00' ],
		#prior : OmReference [ '54' ],
		#self : OmReference [ '55' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 54',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 55',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.939037+01:00' ],
		#prior : OmReference [ '55' ],
		#self : OmReference [ '56' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 55',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 56',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.965695+01:00' ],
		#prior : OmReference [ '56' ],
		#self : OmReference [ '57' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 56',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 57',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:41.990275+01:00' ],
		#prior : OmReference [ '57' ],
		#self : OmReference [ '58' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 57',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 58',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.014613+01:00' ],
		#prior : OmReference [ '58' ],
		#self : OmReference [ '59' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 58',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 59',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.052715+01:00' ],
		#prior : OmReference [ '59' ],
		#self : OmReference [ '60' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 59',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 60',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.084709+01:00' ],
		#prior : OmReference [ '60' ],
		#self : OmReference [ '61' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 60',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 61',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.130068+01:00' ],
		#prior : OmReference [ '61' ],
		#self : OmReference [ '62' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 61',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 62',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.173141+01:00' ],
		#prior : OmReference [ '62' ],
		#self : OmReference [ '63' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 62',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 63',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.210033+01:00' ],
		#prior : OmReference [ '63' ],
		#self : OmReference [ '64' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 63',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 64',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.252573+01:00' ],
		#prior : OmReference [ '64' ],
		#self : OmReference [ '65' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 64',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 65',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.284314+01:00' ],
		#prior : OmReference [ '65' ],
		#self : OmReference [ '66' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 65',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 66',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.31297+01:00' ],
		#prior : OmReference [ '66' ],
		#self : OmReference [ '67' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 66',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 67',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.347075+01:00' ],
		#prior : OmReference [ '67' ],
		#self : OmReference [ '68' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 67',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 68',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.38655+01:00' ],
		#prior : OmReference [ '68' ],
		#self : OmReference [ '69' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 68',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 69',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.43443+01:00' ],
		#prior : OmReference [ '69' ],
		#self : OmReference [ '70' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 69',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 70',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.46296+01:00' ],
		#prior : OmReference [ '70' ],
		#self : OmReference [ '71' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 70',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 71',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.488872+01:00' ],
		#prior : OmReference [ '71' ],
		#self : OmReference [ '72' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 71',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 72',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.515219+01:00' ],
		#prior : OmReference [ '72' ],
		#self : OmReference [ '73' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 72',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 73',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.546986+01:00' ],
		#prior : OmReference [ '73' ],
		#self : OmReference [ '74' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 73',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 74',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.578337+01:00' ],
		#prior : OmReference [ '74' ],
		#self : OmReference [ '75' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 74',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 75',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.606836+01:00' ],
		#prior : OmReference [ '75' ],
		#self : OmReference [ '76' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 75',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 76',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.634362+01:00' ],
		#prior : OmReference [ '76' ],
		#self : OmReference [ '77' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 76',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 77',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.663791+01:00' ],
		#prior : OmReference [ '77' ],
		#self : OmReference [ '78' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 77',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 78',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.688624+01:00' ],
		#prior : OmReference [ '78' ],
		#self : OmReference [ '79' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 78',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 79',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.716865+01:00' ],
		#prior : OmReference [ '79' ],
		#self : OmReference [ '80' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 79',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 80',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.745339+01:00' ],
		#prior : OmReference [ '80' ],
		#self : OmReference [ '81' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 80',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 81',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.769807+01:00' ],
		#prior : OmReference [ '81' ],
		#self : OmReference [ '82' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 81',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 82',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.79895+01:00' ],
		#prior : OmReference [ '82' ],
		#self : OmReference [ '83' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 82',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 83',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.822366+01:00' ],
		#prior : OmReference [ '83' ],
		#self : OmReference [ '84' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 83',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 84',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.85209+01:00' ],
		#prior : OmReference [ '84' ],
		#self : OmReference [ '85' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 84',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 85',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.877044+01:00' ],
		#prior : OmReference [ '85' ],
		#self : OmReference [ '86' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 85',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 86',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.909559+01:00' ],
		#prior : OmReference [ '86' ],
		#self : OmReference [ '87' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 86',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 87',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.938948+01:00' ],
		#prior : OmReference [ '87' ],
		#self : OmReference [ '88' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 87',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 88',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.969068+01:00' ],
		#prior : OmReference [ '88' ],
		#self : OmReference [ '89' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 88',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 89',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:42.994821+01:00' ],
		#prior : OmReference [ '89' ],
		#self : OmReference [ '90' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 89',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 90',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.018665+01:00' ],
		#prior : OmReference [ '90' ],
		#self : OmReference [ '91' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 90',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 91',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.042412+01:00' ],
		#prior : OmReference [ '91' ],
		#self : OmReference [ '92' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 91',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 92',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.065298+01:00' ],
		#prior : OmReference [ '92' ],
		#self : OmReference [ '93' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 92',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 93',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.088744+01:00' ],
		#prior : OmReference [ '93' ],
		#self : OmReference [ '94' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 93',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 94',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.11227+01:00' ],
		#prior : OmReference [ '94' ],
		#self : OmReference [ '95' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 94',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 95',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.13587+01:00' ],
		#prior : OmReference [ '95' ],
		#self : OmReference [ '96' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 95',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 96',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.164257+01:00' ],
		#prior : OmReference [ '96' ],
		#self : OmReference [ '97' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 96',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 97',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.19481+01:00' ],
		#prior : OmReference [ '97' ],
		#self : OmReference [ '98' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 97',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 98',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.229395+01:00' ],
		#prior : OmReference [ '98' ],
		#self : OmReference [ '99' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 98',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 99',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.25674+01:00' ],
		#prior : OmReference [ '99' ],
		#self : OmReference [ '100' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 99',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 100',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.286774+01:00' ],
		#prior : OmReference [ '100' ],
		#self : OmReference [ '101' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 100',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 101',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.318901+01:00' ],
		#prior : OmReference [ '101' ],
		#self : OmReference [ '102' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 101',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 102',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.344416+01:00' ],
		#prior : OmReference [ '102' ],
		#self : OmReference [ '103' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 102',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 103',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.375219+01:00' ],
		#prior : OmReference [ '103' ],
		#self : OmReference [ '104' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 103',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 104',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.404207+01:00' ],
		#prior : OmReference [ '104' ],
		#self : OmReference [ '105' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 104',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 105',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.427796+01:00' ],
		#prior : OmReference [ '105' ],
		#self : OmReference [ '106' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 105',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 106',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.454485+01:00' ],
		#prior : OmReference [ '106' ],
		#self : OmReference [ '107' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 106',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 107',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.484819+01:00' ],
		#prior : OmReference [ '107' ],
		#self : OmReference [ '108' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 107',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 108',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.515167+01:00' ],
		#prior : OmReference [ '108' ],
		#self : OmReference [ '109' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 108',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 109',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.540261+01:00' ],
		#prior : OmReference [ '109' ],
		#self : OmReference [ '110' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 109',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 110',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.56955+01:00' ],
		#prior : OmReference [ '110' ],
		#self : OmReference [ '111' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 110',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 111',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.596315+01:00' ],
		#prior : OmReference [ '111' ],
		#self : OmReference [ '112' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 111',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 112',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.626346+01:00' ],
		#prior : OmReference [ '112' ],
		#self : OmReference [ '113' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 112',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 113',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.650568+01:00' ],
		#prior : OmReference [ '113' ],
		#self : OmReference [ '114' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 113',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 114',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.674795+01:00' ],
		#prior : OmReference [ '114' ],
		#self : OmReference [ '115' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 114',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 115',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.703798+01:00' ],
		#prior : OmReference [ '115' ],
		#self : OmReference [ '116' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 115',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 116',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.733879+01:00' ],
		#prior : OmReference [ '116' ],
		#self : OmReference [ '117' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 116',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 117',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.762784+01:00' ],
		#prior : OmReference [ '117' ],
		#self : OmReference [ '118' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 117',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 118',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.793271+01:00' ],
		#prior : OmReference [ '118' ],
		#self : OmReference [ '119' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 118',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 119',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.821461+01:00' ],
		#prior : OmReference [ '119' ],
		#self : OmReference [ '120' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 119',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 120',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.848903+01:00' ],
		#prior : OmReference [ '120' ],
		#self : OmReference [ '121' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 120',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 121',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.874653+01:00' ],
		#prior : OmReference [ '121' ],
		#self : OmReference [ '122' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 121',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 122',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.898531+01:00' ],
		#prior : OmReference [ '122' ],
		#self : OmReference [ '123' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 122',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 123',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.922354+01:00' ],
		#prior : OmReference [ '123' ],
		#self : OmReference [ '124' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 123',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 124',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.946209+01:00' ],
		#prior : OmReference [ '124' ],
		#self : OmReference [ '125' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 124',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 125',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:43.976064+01:00' ],
		#prior : OmReference [ '125' ],
		#self : OmReference [ '126' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 125',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 126',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.005728+01:00' ],
		#prior : OmReference [ '126' ],
		#self : OmReference [ '127' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 126',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 127',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.0354+01:00' ],
		#prior : OmReference [ '127' ],
		#self : OmReference [ '128' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 127',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 128',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.063857+01:00' ],
		#prior : OmReference [ '128' ],
		#self : OmReference [ '129' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 128',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 129',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.090818+01:00' ],
		#prior : OmReference [ '129' ],
		#self : OmReference [ '130' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 129',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 130',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.122878+01:00' ],
		#prior : OmReference [ '130' ],
		#self : OmReference [ '131' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 130',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 131',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.148814+01:00' ],
		#prior : OmReference [ '131' ],
		#self : OmReference [ '132' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 131',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 132',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.178773+01:00' ],
		#prior : OmReference [ '132' ],
		#self : OmReference [ '133' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 132',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 133',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.204875+01:00' ],
		#prior : OmReference [ '133' ],
		#self : OmReference [ '134' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 133',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 134',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.233139+01:00' ],
		#prior : OmReference [ '134' ],
		#self : OmReference [ '135' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 134',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 135',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.257797+01:00' ],
		#prior : OmReference [ '135' ],
		#self : OmReference [ '136' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 135',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 136',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.292684+01:00' ],
		#prior : OmReference [ '136' ],
		#self : OmReference [ '137' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 136',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 137',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.321263+01:00' ],
		#prior : OmReference [ '137' ],
		#self : OmReference [ '138' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 137',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 138',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.351334+01:00' ],
		#prior : OmReference [ '138' ],
		#self : OmReference [ '139' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 138',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 139',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.383466+01:00' ],
		#prior : OmReference [ '139' ],
		#self : OmReference [ '140' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 139',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 140',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.414262+01:00' ],
		#prior : OmReference [ '140' ],
		#self : OmReference [ '141' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 140',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 141',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.443839+01:00' ],
		#prior : OmReference [ '141' ],
		#self : OmReference [ '142' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 141',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 142',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.467768+01:00' ],
		#prior : OmReference [ '142' ],
		#self : OmReference [ '143' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 142',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 143',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.497104+01:00' ],
		#prior : OmReference [ '143' ],
		#self : OmReference [ '144' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 143',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 144',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.521178+01:00' ],
		#prior : OmReference [ '144' ],
		#self : OmReference [ '145' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 144',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 145',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.54499+01:00' ],
		#prior : OmReference [ '145' ],
		#self : OmReference [ '146' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 145',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 146',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.568995+01:00' ],
		#prior : OmReference [ '146' ],
		#self : OmReference [ '147' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 146',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 147',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.593136+01:00' ],
		#prior : OmReference [ '147' ],
		#self : OmReference [ '148' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 147',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 148',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.623893+01:00' ],
		#prior : OmReference [ '148' ],
		#self : OmReference [ '149' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 148',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 149',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.655981+01:00' ],
		#prior : OmReference [ '149' ],
		#self : OmReference [ '150' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 149',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 150',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.681979+01:00' ],
		#prior : OmReference [ '150' ],
		#self : OmReference [ '151' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 150',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 151',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.712941+01:00' ],
		#prior : OmReference [ '151' ],
		#self : OmReference [ '152' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 151',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 152',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.74151+01:00' ],
		#prior : OmReference [ '152' ],
		#self : OmReference [ '153' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 152',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 153',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.76695+01:00' ],
		#prior : OmReference [ '153' ],
		#self : OmReference [ '154' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 153',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 154',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.798145+01:00' ],
		#prior : OmReference [ '154' ],
		#self : OmReference [ '155' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 154',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 155',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.828531+01:00' ],
		#prior : OmReference [ '155' ],
		#self : OmReference [ '156' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 155',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 156',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.85448+01:00' ],
		#prior : OmReference [ '156' ],
		#self : OmReference [ '157' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 156',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 157',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.887934+01:00' ],
		#prior : OmReference [ '157' ],
		#self : OmReference [ '158' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 157',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 158',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.9166+01:00' ],
		#prior : OmReference [ '158' ],
		#self : OmReference [ '159' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 158',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 159',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.941352+01:00' ],
		#prior : OmReference [ '159' ],
		#self : OmReference [ '160' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 159',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 160',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.971153+01:00' ],
		#prior : OmReference [ '160' ],
		#self : OmReference [ '161' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 160',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 161',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:44.997652+01:00' ],
		#prior : OmReference [ '161' ],
		#self : OmReference [ '162' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 161',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 162',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.032723+01:00' ],
		#prior : OmReference [ '162' ],
		#self : OmReference [ '163' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 162',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 163',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.065149+01:00' ],
		#prior : OmReference [ '163' ],
		#self : OmReference [ '164' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 163',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 164',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.094387+01:00' ],
		#prior : OmReference [ '164' ],
		#self : OmReference [ '165' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 164',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 165',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.120191+01:00' ],
		#prior : OmReference [ '165' ],
		#self : OmReference [ '166' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 165',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 166',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.14458+01:00' ],
		#prior : OmReference [ '166' ],
		#self : OmReference [ '167' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 166',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 167',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.169519+01:00' ],
		#prior : OmReference [ '167' ],
		#self : OmReference [ '168' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 167',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 168',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.196781+01:00' ],
		#prior : OmReference [ '168' ],
		#self : OmReference [ '169' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 168',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 169',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.228094+01:00' ],
		#prior : OmReference [ '169' ],
		#self : OmReference [ '170' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 169',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 170',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.256236+01:00' ],
		#prior : OmReference [ '170' ],
		#self : OmReference [ '171' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 170',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 171',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.286761+01:00' ],
		#prior : OmReference [ '171' ],
		#self : OmReference [ '172' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 171',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 172',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.321317+01:00' ],
		#prior : OmReference [ '172' ],
		#self : OmReference [ '173' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 172',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 173',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.368183+01:00' ],
		#prior : OmReference [ '173' ],
		#self : OmReference [ '174' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 173',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 174',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.407414+01:00' ],
		#prior : OmReference [ '174' ],
		#self : OmReference [ '175' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 174',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 175',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.450764+01:00' ],
		#prior : OmReference [ '175' ],
		#self : OmReference [ '176' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 175',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 176',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.481071+01:00' ],
		#prior : OmReference [ '176' ],
		#self : OmReference [ '177' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 176',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 177',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.51853+01:00' ],
		#prior : OmReference [ '177' ],
		#self : OmReference [ '178' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 177',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 178',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.551908+01:00' ],
		#prior : OmReference [ '178' ],
		#self : OmReference [ '179' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 178',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 179',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.582955+01:00' ],
		#prior : OmReference [ '179' ],
		#self : OmReference [ '180' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 179',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 180',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.609134+01:00' ],
		#prior : OmReference [ '180' ],
		#self : OmReference [ '181' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 180',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 181',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.639196+01:00' ],
		#prior : OmReference [ '181' ],
		#self : OmReference [ '182' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 181',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 182',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.663659+01:00' ],
		#prior : OmReference [ '182' ],
		#self : OmReference [ '183' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 182',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 183',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.6894+01:00' ],
		#prior : OmReference [ '183' ],
		#self : OmReference [ '184' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 183',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 184',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.72257+01:00' ],
		#prior : OmReference [ '184' ],
		#self : OmReference [ '185' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 184',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 185',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.747315+01:00' ],
		#prior : OmReference [ '185' ],
		#self : OmReference [ '186' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 185',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 186',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.777636+01:00' ],
		#prior : OmReference [ '186' ],
		#self : OmReference [ '187' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 186',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 187',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.805229+01:00' ],
		#prior : OmReference [ '187' ],
		#self : OmReference [ '188' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 187',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 188',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.832444+01:00' ],
		#prior : OmReference [ '188' ],
		#self : OmReference [ '189' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 188',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 189',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.857723+01:00' ],
		#prior : OmReference [ '189' ],
		#self : OmReference [ '190' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 189',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 190',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.892219+01:00' ],
		#prior : OmReference [ '190' ],
		#self : OmReference [ '191' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 190',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 191',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.923332+01:00' ],
		#prior : OmReference [ '191' ],
		#self : OmReference [ '192' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 191',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 192',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.954473+01:00' ],
		#prior : OmReference [ '192' ],
		#self : OmReference [ '193' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 192',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 193',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:45.983842+01:00' ],
		#prior : OmReference [ '193' ],
		#self : OmReference [ '194' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 193',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 194',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:46.016343+01:00' ],
		#prior : OmReference [ '194' ],
		#self : OmReference [ '195' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 194',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 195',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:46.048514+01:00' ],
		#prior : OmReference [ '195' ],
		#self : OmReference [ '196' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 195',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 196',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:46.078944+01:00' ],
		#prior : OmReference [ '196' ],
		#self : OmReference [ '197' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 196',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 197',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:46.107468+01:00' ],
		#prior : OmReference [ '197' ],
		#self : OmReference [ '198' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 197',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 198',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:46.132993+01:00' ],
		#prior : OmReference [ '198' ],
		#self : OmReference [ '199' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 198',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 199',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

OmEntry {
	#tags : {
		#author : 'AlistairGrant',
		#time : DateAndTime [ '2023-01-02T14:26:46.16246+01:00' ],
		#prior : OmReference [ '199' ],
		#self : OmReference [ '200' ]
	},
	#content : EpMethodModification {
		#oldMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 199',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		},
		#newMethod : RGMethodDefinition {
			#annotations : IdentityDictionary {
				#className : #OmBlockFileStore,
				#isMetaSide : false
			},
			#name : #internalStateIssue,
			#protocol : #'test data',
			#sourceCode : 'internalStateIssue\r\r\t^ 200',
			#stamp : 'AlistairGrant 1/2/2023 14:26',
			#package : #Ombu
		}
	}
}

"
]

{ #category : 'initialization' }
OmBlockFileStoreTest >> setUpOtherStore [

	^ OmBlockFileStore
		named: 'testingOtherStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory
]

{ #category : 'initialization' }
OmBlockFileStoreTest >> setUpStore [

	^ OmBlockFileStore
		named: 'testingStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory
]

{ #category : 'running' }
OmBlockFileStoreTest >> tearDown [

	bigStore ifNotNil:
		[ bigStore fileReference ensureDelete ].
	super tearDown
]

{ #category : 'accessing' }
OmBlockFileStoreTest >> testEntryForIfPresentIfAbsent [
	"Check that #entryFor:ifPresent:ifAbsent: loads the file past the first block"
	| ombuContent ombuFile mr1 entry mr2 |

	ombuContent := (self class class >> #multiBlockOmbu) comments second.
	ombuFile := self directoryForTests / 'bigStore.ombu'.
	self deny: ombuFile exists.
	ombuFile writeStreamDo: [ :stream |
		stream << ombuContent ].
	bigStore := OmBlockFileStore
		named: 'bigStore'
		inDirectory: ombuFile parent.
	targetReference := OmReference
		globalName: 'bigStore'
		localName: '199'.
	mr1 := bigStore mustRefresh.
	self assert: mr1
		description: 'mustRefresh should be true prior to search'.
	entry := bigStore
		entryFor: targetReference
		ifPresent: [ :found | found ]
		ifAbsent: [ nil ].
	mr2 := bigStore mustRefresh.
	self assert: entry isNotNil
		description: 'entry not found'.
	self assert: mr2 not
		description: 'mustRefresh should be false after to search'
]

{ #category : 'tests' }
OmBlockFileStoreTest >> testMultibyteUnicodeCharacterCrossingTwoBlocks [
	"Test fogbugz case 20112"

	| aWideString |
	aWideString := (String streamContents: [:stream |
		store blockSize timesRepeat: [ stream nextPut: (Character value: 266) ].
	 ]).
	store newEntry: (OmEntry content: aWideString).
	store newEntry: (OmEntry content: aWideString).

	"self shouldnt: [ "store flush" ] raise: OmFileStoreReadingError"
]
