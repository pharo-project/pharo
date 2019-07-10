"
I'm a fake commit that serves as model for Hiedra examples.

The example methods in class-side captured the commits in real git tags.
To capture each one, the following code was inserted into IceTipCommitsDataSource>>elements:

~~~~
[	| commits commitsAsString sourceCode selector |
	commits := elements collect: [:each | 
		HiExampleCommit new
			id: each entity id;
			comment: each entity comment;
			parentIds: each entity ancestorIds;
			yourself ].
	commitsAsString := STON toStringPretty: commits.
	selector := tool model repositoryModel name, '_', ((tool model name copyReplaceAll: '.' with: '_') copyReplaceAll: '-' with: '_').
	sourceCode := selector, '
	^ STON fromString: (', commitsAsString storeString, ')'.
	HiExampleCommit classSide compile: sourceCode classified: #examples. 
] value.

~~~~


"
Class {
	#name : #HiExampleCommit,
	#superclass : #Object,
	#instVars : [
		'id',
		'comment',
		'parentIds'
	],
	#category : #'Hiedra-Examples'
}

{ #category : #examples }
HiExampleCommit class >> iceberg_v1_5_6 [
	^ STON fromString: ('OrderedCollection [
	HiExampleCommit {
		#id : ''a7685ac060591acc96359d8eb84a8efb34d9a5b9'',
		#comment : ''Merge pull request #1152 from estebanlm/update-tonel-to-1-0-13\n\nupdate-tonel-to-1-0-13'',
		#parentIds : [
			''50d6a87e396482cb1d2f9a8b54012db8849e5660'',
			''6eb4ed963e6e929fb96f6f859b4692cfcdb3a9a6''
		]
	},
	HiExampleCommit {
		#id : ''6eb4ed963e6e929fb96f6f859b4692cfcdb3a9a6'',
		#comment : ''tonel updated to 1.0.13'',
		#parentIds : [
			''50d6a87e396482cb1d2f9a8b54012db8849e5660''
		]
	},
	HiExampleCommit {
		#id : ''50d6a87e396482cb1d2f9a8b54012db8849e5660'',
		#comment : ''Use libgit v1.5.5\n - support for win64'',
		#parentIds : [
			''1ca4f1e1b159bfb40ef295bffb2a09a956a4732f''
		]
	},
	HiExampleCommit {
		#id : ''1ca4f1e1b159bfb40ef295bffb2a09a956a4732f'',
		#comment : ''Merge b05fb25013f4bf0dcf7ea35d12bbdf50efab75bc'',
		#parentIds : [
			''4a1ae625d8fb83347db948d14bc2f914d75828d7'',
			''b05fb25013f4bf0dcf7ea35d12bbdf50efab75bc''
		]
	},
	HiExampleCommit {
		#id : ''b05fb25013f4bf0dcf7ea35d12bbdf50efab75bc'',
		#comment : ''Merge pull request #1146 from tesonep/issue/1140-Update-Update-Instructions\n\nUpdating the update instructions'',
		#parentIds : [
			''07f556ae5c49efc1b397478137608cab42b10216'',
			''8633f9e4f0d29405ea4675619401d3f47e9cf5b2''
		]
	},
	HiExampleCommit {
		#id : ''8633f9e4f0d29405ea4675619401d3f47e9cf5b2'',
		#comment : ''Updating the update instructions\n'',
		#parentIds : [
			''1502d34b7061cc3f8029b251779e816d5f4b95d7''
		]
	},
	HiExampleCommit {
		#id : ''4a1ae625d8fb83347db948d14bc2f914d75828d7'',
		#comment : ''Use a do: instead of anySatisfy: because it will shortcircuits the looping on the collection and only treat the first valid package.'',
		#parentIds : [
			''c6b9078f8b73f10ac5cfc9f69c4be60964fe0c14''
		]
	},
	HiExampleCommit {
		#id : ''07f556ae5c49efc1b397478137608cab42b10216'',
		#comment : ''Merge pull request #1144 from pharo-vcs/dev-1.0\n\nPreparing release of v1.5.2'',
		#parentIds : [
			''2daec2208f94e7819f35bd8ae8e01837032cd5ae'',
			''c6b9078f8b73f10ac5cfc9f69c4be60964fe0c14''
		]
	},
	HiExampleCommit {
		#id : ''c6b9078f8b73f10ac5cfc9f69c4be60964fe0c14'',
		#comment : ''Merge pull request #1142 from tesonep/improveMetacelloIntegration\n\nImprove Metacello Integration'',
		#parentIds : [
			''88795ab1f2b7f255f0252542c83e7f68e1e0433d'',
			''a9c74035e32b3dae8ad181efdad7bac58e643c24''
		]
	},
	HiExampleCommit {
		#id : ''88795ab1f2b7f255f0252542c83e7f68e1e0433d'',
		#comment : ''Merge pull request #1143 from tesonep/updateTonel\n\nUpdate tonel'',
		#parentIds : [
			''7089096ee92e364324e9635a57fd9ab994d74a51'',
			''1502d34b7061cc3f8029b251779e816d5f4b95d7''
		]
	},
	HiExampleCommit {
		#id : ''1502d34b7061cc3f8029b251779e816d5f4b95d7'',
		#comment : ''Updating Tonel\n'',
		#parentIds : [
			''37b366812b603cd1cb419db8d7444906efaa6bc8''
		]
	},
	HiExampleCommit {
		#id : ''a9c74035e32b3dae8ad181efdad7bac58e643c24'',
		#comment : ''Fixing the test'',
		#parentIds : [
			''9c8983d1f54b476112763dc59c7af0319e243e8e''
		]
	},
	HiExampleCommit {
		#id : ''7089096ee92e364324e9635a57fd9ab994d74a51'',
		#comment : ''Merge pull request #1085 from jecisc/1084-Review-design-of-outline-datasource\n\n1084-Review-design-of-outline-datasource'',
		#parentIds : [
			''b75b3c4af27fc4937622f6ec0e18289454ccfa6d'',
			''dd171b5805c0d990734d4185e6d2c6880bad2cdc''
		]
	},
	HiExampleCommit {
		#id : ''9c8983d1f54b476112763dc59c7af0319e243e8e'',
		#comment : ''Adding the support to handle different versions of the same project with Metacello.\nIssue  https://pharo.manuscript.com/f/cases/22784'',
		#parentIds : [
			''37b366812b603cd1cb419db8d7444906efaa6bc8''
		]
	},
	HiExampleCommit {
		#id : ''b75b3c4af27fc4937622f6ec0e18289454ccfa6d'',
		#comment : ''Merge pull request #1131 from jecisc/1130-colorModified-should-not-hardcode-color\n\n1130-colorModified-should-not-hardcode-color'',
		#parentIds : [
			''37b366812b603cd1cb419db8d7444906efaa6bc8'',
			''33285d13f7863f47240b2ebb3e393697a5514250''
		]
	},
	HiExampleCommit {
		#id : ''37b366812b603cd1cb419db8d7444906efaa6bc8'',
		#comment : ''Merge pull request #1136 from dionisiydk/1134-should-depends-on-Commander-0-7-2\n\n1134-should-depends-on-Commander-0-7-2'',
		#parentIds : [
			''ca29add011206fcde1c4a36f0514b7e4dc69d3c0'',
			''e5a88f695aebe84bb95e2e309f73e3ab41e59578''
		]
	},
	HiExampleCommit {
		#id : ''ca29add011206fcde1c4a36f0514b7e4dc69d3c0'',
		#comment : ''Merge pull request #1127 from guillep/fix/preload-tonel\n\nLoad tonel as pre-load action'',
		#parentIds : [
			''2daec2208f94e7819f35bd8ae8e01837032cd5ae'',
			''c7627e8a4f79f5c7961379bc199510d6f7431962''
		]
	},
	HiExampleCommit {
		#id : ''e5a88f695aebe84bb95e2e309f73e3ab41e59578'',
		#comment : ''fix Commander version'',
		#parentIds : [
			''2daec2208f94e7819f35bd8ae8e01837032cd5ae''
		]
	},
	HiExampleCommit {
		#id : ''33285d13f7863f47240b2ebb3e393697a5514250'',
		#comment : ''Do not hardcode color modified\n\nFixes #1130'',
		#parentIds : [
			''c25033270d8b25819021c6259b1e612fe9d21ebc''
		]
	},
	HiExampleCommit {
		#id : ''c7627e8a4f79f5c7961379bc199510d6f7431962'',
		#comment : ''Update metacello too\n'',
		#parentIds : [
			''32bbb2c70167ebef120be1e195fed420587cdba4''
		]
	},
	HiExampleCommit {
		#id : ''32bbb2c70167ebef120be1e195fed420587cdba4'',
		#comment : ''Add tonel loading in preload script\n'',
		#parentIds : [
			''2daec2208f94e7819f35bd8ae8e01837032cd5ae''
		]
	},
	HiExampleCommit {
		#id : ''2daec2208f94e7819f35bd8ae8e01837032cd5ae'',
		#comment : ''Merge pull request #1125 from jecisc/1124-Loading-of-Iceberg-is-broken-because-of-Commander\n\n1124-Loading-of-Iceberg-is-broken-because-of-Commander'',
		#parentIds : [
			''66a05ab61e98e3104c5be527acd410c11e8995af'',
			''0e4e74708d7eb2ed1c1e3a7fae6927186ca3ce5d''
		]
	},
	HiExampleCommit {
		#id : ''0e4e74708d7eb2ed1c1e3a7fae6927186ca3ce5d'',
		#comment : ''Commander is now in the src subdirectory.\n\nFixes #1124'',
		#parentIds : [
			''66a05ab61e98e3104c5be527acd410c11e8995af''
		]
	},
	HiExampleCommit {
		#id : ''66a05ab61e98e3104c5be527acd410c11e8995af'',
		#comment : ''Merge branch \''master\'' into dev-1.0\n'',
		#parentIds : [
			''0e98cbc0099908aa5b4b950e863abea009f89e30'',
			''95f2597743d0469ac3642773ae74c92010ae0a4a''
		]
	},
	HiExampleCommit {
		#id : ''0e98cbc0099908aa5b4b950e863abea009f89e30'',
		#comment : ''Merge pull request #1101 from jecisc/1099-Gitlab-integration-should-manage-non-default-port\n\n1099-Gitlab-integration-should-manage-non-default-port'',
		#parentIds : [
			''5e41499781ad29809d9d660b68e8dc34cde66bb1'',
			''788d33ac0743515893f6923b80088dff4f128e00''
		]
	},
	HiExampleCommit {
		#id : ''5e41499781ad29809d9d660b68e8dc34cde66bb1'',
		#comment : ''Merge pull request #1083 from jecisc/1080-Update-to-Commander-v071\n\n1080-Update-to-Commander-v071'',
		#parentIds : [
			''4d8967945a0d94acbe2ff7ef5e2301a7d27e8015'',
			''e3ab6200f335cc928be404e0ec8ec1effa560545''
		]
	},
	HiExampleCommit {
		#id : ''4d8967945a0d94acbe2ff7ef5e2301a7d27e8015'',
		#comment : ''Merge pull request #1094 from vitormcruz/improve-doc\n\nImprove doc'',
		#parentIds : [
			''b1336d632577d9c527ad786de33111c29d0f8f63'',
			''711cf3f08e969903262d00530e4441c136186dd6''
		]
	},
	HiExampleCommit {
		#id : ''95f2597743d0469ac3642773ae74c92010ae0a4a'',
		#comment : ''Merge pull request #1115 from SabineMa/master\n\nUpdate README.md'',
		#parentIds : [
			''a924866005e301b20545101f1b58189d5fa37b3d'',
			''3ef383d73bc53171ae249c80cc3c65caaed26433''
		]
	},
	HiExampleCommit {
		#id : ''b1336d632577d9c527ad786de33111c29d0f8f63'',
		#comment : ''Merge pull request #1109 from guillep/1105-Entering-an-issue-in-create-a-branch-raises-a-DNU\n\n1105-Entering-an-issue-in-create-a-branch-raises-a-DNU'',
		#parentIds : [
			''3bb654a4deda99e1c478d0c4f25acb4925ab507f'',
			''929c9526631a24efe6f2f223d839df715be1d1d0''
		]
	},
	HiExampleCommit {
		#id : ''3ef383d73bc53171ae249c80cc3c65caaed26433'',
		#comment : ''Update README.md\n\nAdd information about loading iceberg in pharo6.1. with authentication'',
		#parentIds : [
			''a924866005e301b20545101f1b58189d5fa37b3d''
		]
	},
	HiExampleCommit {
		#id : ''788d33ac0743515893f6923b80088dff4f128e00'',
		#comment : ''P6.1 compatibility\n'',
		#parentIds : [
			''fb25ae334bcb40656ae2501e1a644c4bf2417cdf''
		]
	},
	HiExampleCommit {
		#id : ''3bb654a4deda99e1c478d0c4f25acb4925ab507f'',
		#comment : ''Merge pull request #1079 from demarey/1074-cannot-checkout-a-tag\n\n1074-cannot-checkout-a-tag'',
		#parentIds : [
			''ad3caa3bf9d80728161769d48a3769d89ee91f78'',
			''3307ed383f2abd62ac54e30cea23f26750c152f6''
		]
	},
	HiExampleCommit {
		#id : ''ad3caa3bf9d80728161769d48a3769d89ee91f78'',
		#comment : ''Merge pull request #1107 from guillep/1106-Moving-a-class-between-packages-not-properly-catched\n\n1106-Moving-a-class-between-packages-not-properly-catched'',
		#parentIds : [
			''865d099a22b8c6355288ab1e4942c3b59f3e29d1'',
			''fcf83e211b3ae77a7cc2db449065514c64a128ff''
		]
	},
	HiExampleCommit {
		#id : ''865d099a22b8c6355288ab1e4942c3b59f3e29d1'',
		#comment : ''Merge pull request #1108 from guillep/1104-pushTo-leads-to-error-on-some-branches\n\n1104-pushTo-leads-to-error-on-some-branches'',
		#parentIds : [
			''c2d420cded07e456f57d20870257d0fd84d8add1'',
			''718bf5df912cf4b479b1c339e17392178007a9dd''
		]
	},
	HiExampleCommit {
		#id : ''c2d420cded07e456f57d20870257d0fd84d8add1'',
		#comment : ''Merge pull request #1098 from dupriezt/dev-1.0\n\nRemoved leftover halts'',
		#parentIds : [
			''688941a8953bbbc6561fdeacd37fb493d186291c'',
			''40a6b54e4a2e5854fbebb11dc6673e9bf4dba812''
		]
	},
	HiExampleCommit {
		#id : ''929c9526631a24efe6f2f223d839df715be1d1d0'',
		#comment : ''Be able to create branches in detached HEAD mode'',
		#parentIds : [
			''688941a8953bbbc6561fdeacd37fb493d186291c''
		]
	},
	HiExampleCommit {
		#id : ''718bf5df912cf4b479b1c339e17392178007a9dd'',
		#comment : ''Fix bug in fetching branches'',
		#parentIds : [
			''528a45f84226c6a0c4d489a47d95b5f5caf73dd4''
		]
	},
	HiExampleCommit {
		#id : ''528a45f84226c6a0c4d489a47d95b5f5caf73dd4'',
		#comment : ''Add failing test for issue #1104'',
		#parentIds : [
			''688941a8953bbbc6561fdeacd37fb493d186291c''
		]
	},
	HiExampleCommit {
		#id : ''fcf83e211b3ae77a7cc2db449065514c64a128ff'',
		#comment : ''Fix issue #1106\nCorrectly ask the announcement for the affected packages and not to the class.'',
		#parentIds : [
			''688941a8953bbbc6561fdeacd37fb493d186291c''
		]
	},
	HiExampleCommit {
		#id : ''fb25ae334bcb40656ae2501e1a644c4bf2417cdf'',
		#comment : ''Add possibility to clone project from self hosted gitlab with non default ssh port  via the Metacello integration.\n\nFixes #1099\n\n/!\\ This needs https://github.com/pharo-project/pharo/pull/2041 before it can work'',
		#parentIds : [
			''c25033270d8b25819021c6259b1e612fe9d21ebc''
		]
	},
	HiExampleCommit {
		#id : ''688941a8953bbbc6561fdeacd37fb493d186291c'',
		#comment : ''Merge pull request #1097 from tesonep/UpdatingTonelTov1.0.10\n\nUpdating Tonel to V1.0.11'',
		#parentIds : [
			''c25033270d8b25819021c6259b1e612fe9d21ebc'',
			''68f33cdefe0d256ad41d08d0d20020e67bc3cc12''
		]
	},
	HiExampleCommit {
		#id : ''40a6b54e4a2e5854fbebb11dc6673e9bf4dba812'',
		#comment : ''Removed leftover halts'',
		#parentIds : [
			''c25033270d8b25819021c6259b1e612fe9d21ebc''
		]
	},
	HiExampleCommit {
		#id : ''68f33cdefe0d256ad41d08d0d20020e67bc3cc12'',
		#comment : ''Updating tonel to v1.0.11\n'',
		#parentIds : [
			''23f36a3ed7e31b08c5d60cd7ccc2c62ecaec3be4''
		]
	},
	HiExampleCommit {
		#id : ''23f36a3ed7e31b08c5d60cd7ccc2c62ecaec3be4'',
		#comment : ''Updating Tonel to V1.0.10\n'',
		#parentIds : [
			''c25033270d8b25819021c6259b1e612fe9d21ebc''
		]
	},
	HiExampleCommit {
		#id : ''711cf3f08e969903262d00530e4441c136186dd6'',
		#comment : ''Explaining problem with Windows 10 longpath\n'',
		#parentIds : [
			''0c50966b989367a2f3f9537d7517372ed3d89dcb''
		]
	},
	HiExampleCommit {
		#id : ''0c50966b989367a2f3f9537d7517372ed3d89dcb'',
		#comment : ''Merge branch \''master\'' of https://github.com/pharo-vcs/iceberg\n'',
		#parentIds : [
			''49e29134cb68febf31b454e16a0818528e01df4c'',
			''a924866005e301b20545101f1b58189d5fa37b3d''
		]
	},
	HiExampleCommit {
		#id : ''dd171b5805c0d990734d4185e6d2c6880bad2cdc'',
		#comment : ''Add number of commits to the right of the outline header cell'',
		#parentIds : [
			''5544a1f4942a1dd0a08877638e2a4e31bce2669f''
		]
	},
	HiExampleCommit {
		#id : ''5544a1f4942a1dd0a08877638e2a4e31bce2669f'',
		#comment : ''Restore outline datasource and improve the design/features.\n\nFixes #1084'',
		#parentIds : [
			''c25033270d8b25819021c6259b1e612fe9d21ebc''
		]
	},
	HiExampleCommit {
		#id : ''e3ab6200f335cc928be404e0ec8ec1effa560545'',
		#comment : ''Update to Commander v0.7.1\n\nFixes #1080'',
		#parentIds : [
			''c25033270d8b25819021c6259b1e612fe9d21ebc''
		]
	},
	HiExampleCommit {
		#id : ''c25033270d8b25819021c6259b1e612fe9d21ebc'',
		#comment : ''Use Libgit 1.5.4'',
		#parentIds : [
			''7fe0d7caa7d1bf48510ec47d0c4fe0dd61574e71''
		]
	},
	HiExampleCommit {
		#id : ''7fe0d7caa7d1bf48510ec47d0c4fe0dd61574e71'',
		#comment : ''Merge pull request #1072 from jecisc/1067-IceTipHistoryDiffUpdater-model-variable-seems-useless\n\n1067-IceTipHistoryDiffUpdater-model-variable-seems-useless'',
		#parentIds : [
			''6cc68008fbcbed26bb20ef7af447ed7b2c8706c2'',
			''129a4b05e9d957725dfaa54a52809cbe8788b9b0''
		]
	},
	HiExampleCommit {
		#id : ''129a4b05e9d957725dfaa54a52809cbe8788b9b0'',
		#comment : ''Merge branch \''dev-1.0\'' into 1067-IceTipHistoryDiffUpdater-model-variable-seems-useless'',
		#parentIds : [
			''b8ca3ed14db6959fe7c056ad494ed20bbea05493'',
			''6cc68008fbcbed26bb20ef7af447ed7b2c8706c2''
		]
	},
	HiExampleCommit {
		#id : ''6cc68008fbcbed26bb20ef7af447ed7b2c8706c2'',
		#comment : ''Merge pull request #1078 from jecisc/1077-Repository-view-Allow-to-collapse-branchesremotestags-trees\n\n1077-Repository-view-Allow-to-collapse-branchesremotestags-trees'',
		#parentIds : [
			''863eaeb0b3131b84c1e3fd8193533e36e7da6811'',
			''ad848f36fee4fd431bd7bfebf32e717bc82cf0e5''
		]
	},
	HiExampleCommit {
		#id : ''3307ed383f2abd62ac54e30cea23f26750c152f6'',
		#comment : ''Add command + UI menu to checkout a tag (from the repository view)'',
		#parentIds : [
			''57fbabb9afb7f52d3e31f187fe8e184b18b40180''
		]
	},
	HiExampleCommit {
		#id : ''863eaeb0b3131b84c1e3fd8193533e36e7da6811'',
		#comment : ''Merge pull request #1075 from jecisc/847-Move-tags-under-remotes-in-Repository-view\n\n847-Move-tags-under-remotes-in-Repository-view'',
		#parentIds : [
			''2971fac6a536480e32bc072202be1a1611b55577'',
			''2640e0833d233968e13bf394c28e68fcc8322e09''
		]
	},
	HiExampleCommit {
		#id : ''2971fac6a536480e32bc072202be1a1611b55577'',
		#comment : ''Merge pull request #1071 from jecisc/1015-Replace-usages-of-Glamour-in-the-Github-Plugin\n\n1015-Replace-usages-of-Glamour-in-the-Github-Plugin'',
		#parentIds : [
			''1ec5c6f6aeb5f285d9bf523fee1135ffc15877ec'',
			''f089b835bc7605500ddce24d90ec56f7b21cb283''
		]
	},
	HiExampleCommit {
		#id : ''1ec5c6f6aeb5f285d9bf523fee1135ffc15877ec'',
		#comment : ''Merge pull request #1070 from pharo-vcs/859-set-upstream\n\nset upstream if missing'',
		#parentIds : [
			''df27cfe051c715137c6991702fd642f3f08cc6de'',
			''f787222601e45798241059a5d8f6f363f2157144''
		]
	},
	HiExampleCommit {
		#id : ''df27cfe051c715137c6991702fd642f3f08cc6de'',
		#comment : ''Merge pull request #1063 from jecisc/1061-Introduce-iconNamed-in-IceDefinition-and-IceTipModel-and-remove-all-the-terrible-Smalltalk-ui-icons\n\n1061-Introduce-iconNamed-in-IceDefinition-and-IceTipModel-and-remove-all-the-terrible-Smalltalk-ui-icons'',
		#parentIds : [
			''faf780c0f2249d4ea33321f3f357739a03771a64'',
			''b3dc111607459ee91c86088b810b1a74bd437120''
		]
	},
	HiExampleCommit {
		#id : ''ad848f36fee4fd431bd7bfebf32e717bc82cf0e5'',
		#comment : ''Allow to collapse branch/remote/tags trees in repository view. \n\nWhen we are working on  a project with a lot of branches/tags we might benefit from been able to collapse a section. \n\n#Fixes 1077'',
		#parentIds : [
			''faf780c0f2249d4ea33321f3f357739a03771a64''
		]
	},
	HiExampleCommit {
		#id : ''2640e0833d233968e13bf394c28e68fcc8322e09'',
		#comment : ''Swap remotes and tags in repository browser.\n\nIn general we have less remotes than tags for big repos.It wake it easier to work on big repos like that.\n\nFixes #847'',
		#parentIds : [
			''faf780c0f2249d4ea33321f3f357739a03771a64''
		]
	},
	HiExampleCommit {
		#id : ''57fbabb9afb7f52d3e31f187fe8e184b18b40180'',
		#comment : ''Add missing fileSystem command used by checkout'',
		#parentIds : [
			''ffd81763a5a35abd58963e559ff9b35818699317''
		]
	},
	HiExampleCommit {
		#id : ''ffd81763a5a35abd58963e559ff9b35818699317'',
		#comment : ''add a print on method on tags so that we can now the tag quickly when inspecting tags'',
		#parentIds : [
			''0528b254e6ba3db7a819bc0dee0861a42bbebc8c''
		]
	},
	HiExampleCommit {
		#id : ''b8ca3ed14db6959fe7c056ad494ed20bbea05493'',
		#comment : ''Remove IceTipHistoryDiffUpdater model variable.\n\nFixes #1067'',
		#parentIds : [
			''faf780c0f2249d4ea33321f3f357739a03771a64''
		]
	},
	HiExampleCommit {
		#id : ''f089b835bc7605500ddce24d90ec56f7b21cb283'',
		#comment : ''Remove last glamour browsers. \n\nI also speed them up.\n\nFixes #1015'',
		#parentIds : [
			''73d7e57e3700c29c73146a19878ca7e47d365dd8''
		]
	},
	HiExampleCommit {
		#id : ''faf780c0f2249d4ea33321f3f357739a03771a64'',
		#comment : ''Fix #1066\nAdd Manifest tag to manifests\n\nhttps://pharo.fogbugz.com/f/cases/22630/\nhttps://pharo.fogbugz.com/f/cases/22436/PackageManifest-subclasses-should-be-packaged-with-Manifest-tag'',
		#parentIds : [
			''c03d2c25fce617fa4743b10e54e990da50df56d0''
		]
	},
	HiExampleCommit {
		#id : ''c03d2c25fce617fa4743b10e54e990da50df56d0'',
		#comment : ''Fix #1068'',
		#parentIds : [
			''0528b254e6ba3db7a819bc0dee0861a42bbebc8c''
		]
	},
	HiExampleCommit {
		#id : ''0528b254e6ba3db7a819bc0dee0861a42bbebc8c'',
		#comment : ''Fix issue #1064'',
		#parentIds : [
			''73d7e57e3700c29c73146a19878ca7e47d365dd8''
		]
	},
	HiExampleCommit {
		#id : ''b3dc111607459ee91c86088b810b1a74bd437120'',
		#comment : ''Remove a usage of `Smalltalk ui icons`'',
		#parentIds : [
			''94d446ddb9381823775bc8d631f986e9ac920fac''
		]
	},
	HiExampleCommit {
		#id : ''94d446ddb9381823775bc8d631f986e9ac920fac'',
		#comment : ''This PR remove a lot of the usage of Object>>iconNamed: \n\nIt does not remove everything but this is a first pass.'',
		#parentIds : [
			''73d7e57e3700c29c73146a19878ca7e47d365dd8''
		]
	},
	HiExampleCommit {
		#id : ''73d7e57e3700c29c73146a19878ca7e47d365dd8'',
		#comment : ''Merge pull request #1054 from jecisc/1036-Debugger-if-we-try-to-merge-without-selecting-a-branch\n\n1036-Debugger-if-we-try-to-merge-without-selecting-a-branch'',
		#parentIds : [
			''ddc668243ccaedaad4d8acb9d719b0fbfde74e31'',
			''eea1d7555ae504fa94622dc48dfaef88dbded836''
		]
	},
	HiExampleCommit {
		#id : ''eea1d7555ae504fa94622dc48dfaef88dbded836'',
		#comment : ''Remove duplication + add comments + add printons'',
		#parentIds : [
			''cc206ae545c0bfae6f40c0ef6111174901fa44e2''
		]
	},
	HiExampleCommit {
		#id : ''ddc668243ccaedaad4d8acb9d719b0fbfde74e31'',
		#comment : ''Merge pull request #1024 from guillep/issue/1023\n\nIssue/1023'',
		#parentIds : [
			''dbfa9bee520866cf57f891106932944b90fd55c8'',
			''ebf66bb8a571527d0727644ffff2e8ead95508ce''
		]
	},
	HiExampleCommit {
		#id : ''ebf66bb8a571527d0727644ffff2e8ead95508ce'',
		#comment : ''Making full source directory structure in tree'',
		#parentIds : [
			''64484954bdcab00963c836c6cf7f11fdd7f84814''
		]
	},
	HiExampleCommit {
		#id : ''cc206ae545c0bfae6f40c0ef6111174901fa44e2'',
		#comment : ''Improve robustess and UX of merge dialog\n\n- Do no throw a MNU if there is no selected branch\n- Select a branch by default if there is one present (either the head of the first one)\n\nFixes #1036'',
		#parentIds : [
			''8d5878e20705f1a939c9b9ed24659271a2dcaeee''
		]
	},
	HiExampleCommit {
		#id : ''dbfa9bee520866cf57f891106932944b90fd55c8'',
		#comment : ''Merge pull request #1053 from jecisc/775-Reviewing-a-PR-is-broken\n\n775-Reviewing-a-PR-is-broken'',
		#parentIds : [
			''8d5878e20705f1a939c9b9ed24659271a2dcaeee'',
			''3cd4ff7204b29780d0ebe81628f96818e3860aa8''
		]
	},
	HiExampleCommit {
		#id : ''64484954bdcab00963c836c6cf7f11fdd7f84814'',
		#comment : ''Merge branch \''dev-1.0\'' into issue/1023'',
		#parentIds : [
			''17fca771021a72055dbc79ecbcd35a8dfa96133c'',
			''8d5878e20705f1a939c9b9ed24659271a2dcaeee''
		]
	},
	HiExampleCommit {
		#id : ''17fca771021a72055dbc79ecbcd35a8dfa96133c'',
		#comment : ''Should reuse same location'',
		#parentIds : [
			''592e79d7bc13ea6407fe1657e95608e0ddac1cb2''
		]
	},
	HiExampleCommit {
		#id : ''592e79d7bc13ea6407fe1657e95608e0ddac1cb2'',
		#comment : ''Using a more stable version of libgit'',
		#parentIds : [
			''a0b3e20d3f90a45f715308b3f1774f0c0d655d27''
		]
	},
	HiExampleCommit {
		#id : ''8d5878e20705f1a939c9b9ed24659271a2dcaeee'',
		#comment : ''Merge b65c12c582a8ebcefd201162cc5ebedf8f56d604'',
		#parentIds : [
			''aa1ff614f2eaf33627fbe863842e34d5d0161b84'',
			''b65c12c582a8ebcefd201162cc5ebedf8f56d604''
		]
	},
	HiExampleCommit {
		#id : ''aa1ff614f2eaf33627fbe863842e34d5d0161b84'',
		#comment : ''Fix issue #991'',
		#parentIds : [
			''1f44844b9335cf6260c0d3355b1ae8c15fff5233''
		]
	},
	HiExampleCommit {
		#id : ''a0b3e20d3f90a45f715308b3f1774f0c0d655d27'',
		#comment : ''Avoid leaking open files in windows'',
		#parentIds : [
			''21342f15e34f0abda489b826a1b205b37eef9a53''
		]
	},
	HiExampleCommit {
		#id : ''3cd4ff7204b29780d0ebe81628f96818e3860aa8'',
		#comment : ''Merge b65c12c582a8ebcefd201162cc5ebedf8f56d604'',
		#parentIds : [
			''b357490664551c5cc7f0bb6e99ce0ab48d8e62fb'',
			''b65c12c582a8ebcefd201162cc5ebedf8f56d604''
		]
	},
	HiExampleCommit {
		#id : ''b65c12c582a8ebcefd201162cc5ebedf8f56d604'',
		#comment : ''Merge pull request #1052 from jecisc/1051-Clean-useless-huge-hierarchy-in-Github-plugin-UI\n\n1051-Clean-useless-huge-hierarchy-in-Github-plugin-UI'',
		#parentIds : [
			''1cd643f2924ff0199e9691de0f07e23f98a48eac'',
			''6dea210abd128215146b6717a198fce4a60ba9a0''
		]
	},
	HiExampleCommit {
		#id : ''b357490664551c5cc7f0bb6e99ce0ab48d8e62fb'',
		#comment : ''Some more missing methods'',
		#parentIds : [
			''b58479e9e889dc3667fb2d27a1a0d293950258c8''
		]
	},
	HiExampleCommit {
		#id : ''b58479e9e889dc3667fb2d27a1a0d293950258c8'',
		#comment : ''Restaure the PRÂ review tool. Is still in Glamour but at least it works.\n\nFixes #775'',
		#parentIds : [
			''1846a2dce520eb6c71b65b738c701d1dc19832bc''
		]
	},
	HiExampleCommit {
		#id : ''1846a2dce520eb6c71b65b738c701d1dc19832bc'',
		#comment : ''Merge 1cd643f2924ff0199e9691de0f07e23f98a48eac'',
		#parentIds : [
			''6dea210abd128215146b6717a198fce4a60ba9a0'',
			''1cd643f2924ff0199e9691de0f07e23f98a48eac''
		]
	},
	HiExampleCommit {
		#id : ''6dea210abd128215146b6717a198fce4a60ba9a0'',
		#comment : ''Remove dead code and useless huge hierarchy.\n\nFixes #1051'',
		#parentIds : [
			''3aecb856d5b4ae1d9b7f22dd0845df523924402a''
		]
	},
	HiExampleCommit {
		#id : ''1cd643f2924ff0199e9691de0f07e23f98a48eac'',
		#comment : ''Merge pull request #1047 from jecisc/1044-Show-a-button-View-on-Github-when-creating-a-PR\n\n1044-Show-a-button-View-on-Github-when-creating-a-PR'',
		#parentIds : [
			''a8e4fe06b7f5c03fe79d18abe98b35dc0a252dba'',
			''25242c68aeaf882cf310dba3add8d37c71792406''
		]
	},
	HiExampleCommit {
		#id : ''a8e4fe06b7f5c03fe79d18abe98b35dc0a252dba'',
		#comment : ''Merge pull request #1049 from jecisc/1048-Add-a-GitHub-option-open-on-github\n\n1048-Add-a-GitHub-option-open-on-github'',
		#parentIds : [
			''1f44844b9335cf6260c0d3355b1ae8c15fff5233'',
			''9356f4e017857fca3dce04a219874fc6a86e648e''
		]
	},
	HiExampleCommit {
		#id : ''21342f15e34f0abda489b826a1b205b37eef9a53'',
		#comment : ''Use canonical repository of ProcessWrapper and version 1.2'',
		#parentIds : [
			''429fbcaf92920a2bc3f1095793fc5e79486f48b8''
		]
	},
	HiExampleCommit {
		#id : ''1f44844b9335cf6260c0d3355b1ae8c15fff5233'',
		#comment : ''Fix #1043\n\n- Tonel packages should not start only with uppercase\n- added tests\n- refactored test fixtures to have configurable package names'',
		#parentIds : [
			''8ae3b42803960d12d61a99813dc9b87287a474be''
		]
	},
	HiExampleCommit {
		#id : ''25242c68aeaf882cf310dba3add8d37c71792406'',
		#comment : ''Better error report.'',
		#parentIds : [
			''21d5e5b2b18e0f24b1bf1d628b795b8c35cef7cd''
		]
	},
	HiExampleCommit {
		#id : ''9356f4e017857fca3dce04a219874fc6a86e648e'',
		#comment : ''More explicit error.'',
		#parentIds : [
			''c909e993a2f8535aa2ee13329e74aa9a4a6ae925''
		]
	},
	HiExampleCommit {
		#id : ''c909e993a2f8535aa2ee13329e74aa9a4a6ae925'',
		#comment : ''Add commands to open remotes on Github directly.\n\nFixes #1048'',
		#parentIds : [
			''3aecb856d5b4ae1d9b7f22dd0845df523924402a''
		]
	},
	HiExampleCommit {
		#id : ''21d5e5b2b18e0f24b1bf1d628b795b8c35cef7cd'',
		#comment : ''When creating a PR we now have the possibility to open the PR on GitHub.\n\nFixes #1044'',
		#parentIds : [
			''3aecb856d5b4ae1d9b7f22dd0845df523924402a''
		]
	},
	HiExampleCommit {
		#id : ''8ae3b42803960d12d61a99813dc9b87287a474be'',
		#comment : ''Fix issue #1034\nMake passprase password type input'',
		#parentIds : [
			''d0cd950c1f151ad5074ba2cb857372fa6beef896''
		]
	},
	HiExampleCommit {
		#id : ''d0cd950c1f151ad5074ba2cb857372fa6beef896'',
		#comment : ''Fix issue #1041\nApply lowecase both to repository name AND pattern'',
		#parentIds : [
			''3aecb856d5b4ae1d9b7f22dd0845df523924402a''
		]
	},
	HiExampleCommit {
		#id : ''3aecb856d5b4ae1d9b7f22dd0845df523924402a'',
		#comment : ''Merge pull request #1022 from guillep/issue/1019\n\nIssue/1019'',
		#parentIds : [
			''f892160792bda397b756f6f98eea19bed7a275b3'',
			''68045f76b0663f26384c67106f227e8b665b3670''
		]
	},
	HiExampleCommit {
		#id : ''f892160792bda397b756f6f98eea19bed7a275b3'',
		#comment : ''Merge pull request #1025 from estebanlm/1008-add-create-branch-from-GitHub-issue-option\n\n1008-add-create-branch-from-GitHub-issue-option'',
		#parentIds : [
			''f637da6df12d81ad2ef27098f94cc2f02648393b'',
			''f9ad5da26e0abdccbc49978f521c37bf4dc45e2d''
		]
	},
	HiExampleCommit {
		#id : ''a924866005e301b20545101f1b58189d5fa37b3d'',
		#comment : ''Merge pull request #1035 from pharo-vcs/859-set-upstream\n\n859-set-upstream'',
		#parentIds : [
			''a0a056747a6da931aec569d7c44d20556441c164'',
			''f787222601e45798241059a5d8f6f363f2157144''
		]
	},
	HiExampleCommit {
		#id : ''f637da6df12d81ad2ef27098f94cc2f02648393b'',
		#comment : ''Merge pull request #1046 from jecisc/1045-fix-tests-in-Pharo6\n\nAdd ByteArray>>hexDumpOn:max: in Pharo 6 package.'',
		#parentIds : [
			''7fb9dcecf0c85a9e69495146c5dd0bf29b658de6'',
			''c23804c432b30d2fc1e7e09643a10e2b2e76aa25''
		]
	},
	HiExampleCommit {
		#id : ''c23804c432b30d2fc1e7e09643a10e2b2e76aa25'',
		#comment : ''Add ByteArray>>hexDumpOn:max: in Pharo 6 package.'',
		#parentIds : [
			''7fb9dcecf0c85a9e69495146c5dd0bf29b658de6''
		]
	},
	HiExampleCommit {
		#id : ''f787222601e45798241059a5d8f6f363f2157144'',
		#comment : ''set upstream if missing'',
		#parentIds : [
			''a39ee7f7adf156ee3ee6007decca04c2c45371fb''
		]
	},
	HiExampleCommit {
		#id : ''7fb9dcecf0c85a9e69495146c5dd0bf29b658de6'',
		#comment : ''Fix #1030\n - add support to display hex dumps of binary files in IceFileDefinition\n - fix GitWorkingCopyUpdater to correctly write only binary contents\n - Fix IceTipDiffPanel so it does first clean the diff before updating it (when clicking back and forth between different files in the diff)\n - add tests'',
		#parentIds : [
			''9ac509bd146daf4e54024a490668155a28ab9b80''
		]
	},
	HiExampleCommit {
		#id : ''9ac509bd146daf4e54024a490668155a28ab9b80'',
		#comment : ''Merge pull request #1026 from peteruhnak/Issue/1010\n\nAdded Recalculate dirty packages command context menu command.'',
		#parentIds : [
			''15c506edfa527f61d906769ce76d1eb6d50dacaa'',
			''fad528ceecdc7e5b11c325247a0f1a4cdd7881dc''
		]
	},
	HiExampleCommit {
		#id : ''15c506edfa527f61d906769ce76d1eb6d50dacaa'',
		#comment : ''Merge pull request #1027 from peteruhnak/improved-tag-dialog\n\nImproved new tag dialog'',
		#parentIds : [
			''07e3403c14d5b435cb82281576a56ed0a72d410f'',
			''4d1cfc7f186db82b2937ad71f1b7d06173c7c633''
		]
	},
	HiExampleCommit {
		#id : ''a0a056747a6da931aec569d7c44d20556441c164'',
		#comment : ''Merge pull request #1028 from santana/docfix\n\ntypos'',
		#parentIds : [
			''a39ee7f7adf156ee3ee6007decca04c2c45371fb'',
			''d99c0d804e1f6bf93508aec6abea27b0b6a982ff''
		]
	},
	HiExampleCommit {
		#id : ''d99c0d804e1f6bf93508aec6abea27b0b6a982ff'',
		#comment : ''typos\n'',
		#parentIds : [
			''a39ee7f7adf156ee3ee6007decca04c2c45371fb''
		]
	},
	HiExampleCommit {
		#id : ''4d1cfc7f186db82b2937ad71f1b7d06173c7c633'',
		#comment : ''Be more permissive on parsing semver parts.'',
		#parentIds : [
			''0f2367c58364185cff9536d6a8a72a4b7bb6dfe6''
		]
	},
	HiExampleCommit {
		#id : ''0f2367c58364185cff9536d6a8a72a4b7bb6dfe6'',
		#comment : ''Improved new tag dialog to show current tags & offer easy semver creation.'',
		#parentIds : [
			''07e3403c14d5b435cb82281576a56ed0a72d410f''
		]
	},
	HiExampleCommit {
		#id : ''fad528ceecdc7e5b11c325247a0f1a4cdd7881dc'',
		#comment : ''Added Recalculate dirty packages command context menu command.\n\nfixes #1010'',
		#parentIds : [
			''07e3403c14d5b435cb82281576a56ed0a72d410f''
		]
	},
	HiExampleCommit {
		#id : ''f9ad5da26e0abdccbc49978f521c37bf4dc45e2d'',
		#comment : ''fix width problem (by cleaning) \nchange icon. \ntitle can be the same as for pharo because it will go away.  '',
		#parentIds : [
			''7a6efe7778a27e37294fe26be44405cdf4076a6f''
		]
	},
	HiExampleCommit {
		#id : ''7a6efe7778a27e37294fe26be44405cdf4076a6f'',
		#comment : ''Merge 07e3403c14d5b435cb82281576a56ed0a72d410f'',
		#parentIds : [
			''e9bc3fab57e3679f769532b10add60d08e712546'',
			''07e3403c14d5b435cb82281576a56ed0a72d410f''
		]
	},
	HiExampleCommit {
		#id : ''429fbcaf92920a2bc3f1095793fc5e79486f48b8'',
		#comment : ''Avoid repeating locations between tests to capture real error'',
		#parentIds : [
			''fa57564a0c7b8db02a622a4d0698f24652c15d26''
		]
	},
	HiExampleCommit {
		#id : ''fa57564a0c7b8db02a622a4d0698f24652c15d26'',
		#comment : ''Try freeing repository manually on tests'',
		#parentIds : [
			''2e7c086058b2acdaded09671d0af07c58abe68ec''
		]
	},
	HiExampleCommit {
		#id : ''2e7c086058b2acdaded09671d0af07c58abe68ec'',
		#comment : ''Try collecting open files'',
		#parentIds : [
			''554182c79ccf3a28c3bb7f181d246875c466ce1d''
		]
	},
	HiExampleCommit {
		#id : ''554182c79ccf3a28c3bb7f181d246875c466ce1d'',
		#comment : ''Add Appveyor CI env var\n'',
		#parentIds : [
			''fe3e28cf279ee14e797365f44599b39e00715d4c''
		]
	},
	HiExampleCommit {
		#id : ''07e3403c14d5b435cb82281576a56ed0a72d410f'',
		#comment : ''Merge pull request #1021 from gcotelli/self_hosted_gitlab\n\nSelf hosted gitlab support'',
		#parentIds : [
			''f019b545e256587ba57e9f3bfddb557d24a53eb6'',
			''536aae20142d4f044e3a01b7e7b6b3c76ed91a15''
		]
	},
	HiExampleCommit {
		#id : ''fe3e28cf279ee14e797365f44599b39e00715d4c'',
		#comment : ''Make CI env var case insensitive following appveyor\''s documentation\ncf. https://www.appveyor.com/docs/environment-variables/'',
		#parentIds : [
			''53a7e052df3f87c495260db0f9179da449fd4223''
		]
	},
	HiExampleCommit {
		#id : ''68045f76b0663f26384c67106f227e8b665b3670'',
		#comment : ''Cleanups'',
		#parentIds : [
			''4927d029b69494db832928a52d23f77dfbb6c779''
		]
	},
	HiExampleCommit {
		#id : ''4927d029b69494db832928a52d23f77dfbb6c779'',
		#comment : ''Do not register any repository into the monticello repository list'',
		#parentIds : [
			''53a7e052df3f87c495260db0f9179da449fd4223''
		]
	},
	HiExampleCommit {
		#id : ''536aae20142d4f044e3a01b7e7b6b3c76ed91a15'',
		#comment : ''Skip self-hosted Gitlab tests for Pharo 6\n'',
		#parentIds : [
			''7b0e2ba35d31ce3aecac8a1b359a1d1e00b42e22''
		]
	},
	HiExampleCommit {
		#id : ''7b0e2ba35d31ce3aecac8a1b359a1d1e00b42e22'',
		#comment : ''Backwards compatibility with Pharo 6\n'',
		#parentIds : [
			''db91602fea78c58af2c6922494e48864d50a75ab''
		]
	},
	HiExampleCommit {
		#id : ''db91602fea78c58af2c6922494e48864d50a75ab'',
		#comment : ''Update scpUrl and httpsUrl to support self-hosted gitlab instances'',
		#parentIds : [
			''f019b545e256587ba57e9f3bfddb557d24a53eb6''
		]
	},
	HiExampleCommit {
		#id : ''f019b545e256587ba57e9f3bfddb557d24a53eb6'',
		#comment : ''Merge pull request #1020 from jecisc/doNotUseDeprecatedClasses\n\n1018-doNotUseDeprecatedClasses'',
		#parentIds : [
			''eb98f8f42e71f8671bd283dc4ce9d0d7a146966d'',
			''43f56b70da7a47ac03cf83822315b94bcabf2648''
		]
	},
	HiExampleCommit {
		#id : ''43f56b70da7a47ac03cf83822315b94bcabf2648'',
		#comment : ''Remove use of Spec deprecated classes'',
		#parentIds : [
			''eb98f8f42e71f8671bd283dc4ce9d0d7a146966d''
		]
	},
	HiExampleCommit {
		#id : ''eb98f8f42e71f8671bd283dc4ce9d0d7a146966d'',
		#comment : ''Merge pull request #1013 from guillep/issue/984\n\nissue/984'',
		#parentIds : [
			''53a7e052df3f87c495260db0f9179da449fd4223'',
			''6ca2094248da0330a4e9399f291d4f2ccf1dff6e''
		]
	},
	HiExampleCommit {
		#id : ''6ca2094248da0330a4e9399f291d4f2ccf1dff6e'',
		#comment : ''Make tests independent of formatting'',
		#parentIds : [
			''3b18204ef8fe0857eb41093415dba7bf93c7fbce''
		]
	},
	HiExampleCommit {
		#id : ''53a7e052df3f87c495260db0f9179da449fd4223'',
		#comment : ''Merge pull request #1011 from guillep/cleanup/977\n\nAdd comments and #defaultHelp to commands'',
		#parentIds : [
			''b0a123b57de0ff5780e523617162b8ab7fd1f973'',
			''715f782b2fab59a0b33ca63056f244476aed0f9b''
		]
	},
	HiExampleCommit {
		#id : ''3b18204ef8fe0857eb41093415dba7bf93c7fbce'',
		#comment : ''Fix issue #984\n - add test for file corruption\n - fix properties file writing'',
		#parentIds : [
			''1066e8446addcb428d5e0fe8bce76cda55bdc6c4''
		]
	},
	HiExampleCommit {
		#id : ''715f782b2fab59a0b33ca63056f244476aed0f9b'',
		#comment : ''Adding class comments'',
		#parentIds : [
			''542155f19fe82572904f35463b06dfddcfc2a4c1''
		]
	},
	HiExampleCommit {
		#id : ''542155f19fe82572904f35463b06dfddcfc2a4c1'',
		#comment : ''- Using non deprecated Commander classes\n- Add comments and defaultHelp'',
		#parentIds : [
			''3925fa75cf2741748009f4675f8b55769b4f2ccd''
		]
	},
	HiExampleCommit {
		#id : ''3925fa75cf2741748009f4675f8b55769b4f2ccd'',
		#comment : ''Pass on package commands'',
		#parentIds : [
			''ddca237e3a1b031e540497164c8e4c96c90cb770''
		]
	},
	HiExampleCommit {
		#id : ''ddca237e3a1b031e540497164c8e4c96c90cb770'',
		#comment : ''Cleanups and comments in a third of the commands of IceTip'',
		#parentIds : [
			''b0a123b57de0ff5780e523617162b8ab7fd1f973''
		]
	},
	HiExampleCommit {
		#id : ''b0a123b57de0ff5780e523617162b8ab7fd1f973'',
		#comment : ''Merge pull request #993 from estebanlm/992-fix-tonel-migration\n\n992-fix-tonel-migration'',
		#parentIds : [
			''f3d6044bf75a743641a566775234633699fd91a7'',
			''baac813e4aa2c541739808427bf6aa0d6b729f48''
		]
	},
	HiExampleCommit {
		#id : ''e9bc3fab57e3679f769532b10add60d08e712546'',
		#comment : ''first version working (still UI cleanups required)'',
		#parentIds : [
			''1d28653affbfda8e942142ca8eed55a580f527b9''
		]
	},
	HiExampleCommit {
		#id : ''f3d6044bf75a743641a566775234633699fd91a7'',
		#comment : ''Merge pull request #1005 from pavel-krivanek/issue1004-displayScaleFactor\n\nissue1004-displayScaleFactor'',
		#parentIds : [
			''bfefed0549173509cf29947d11c0656825ac024a'',
			''86b6780af59c8232885cc4d8d2016b22b438d8f4''
		]
	},
	HiExampleCommit {
		#id : ''86b6780af59c8232885cc4d8d2016b22b438d8f4'',
		#comment : ''IceTipReadOnlyForm scaling'',
		#parentIds : [
			''0c1ac23bcb159011daf90e41ea19a296121c0954''
		]
	},
	HiExampleCommit {
		#id : ''0c1ac23bcb159011daf90e41ea19a296121c0954'',
		#comment : ''Merge branch \''dev-1.0\'' into issue1004-displayScaleFactor'',
		#parentIds : [
			''e93fc5ac8da85aa1d63b1ad3ecb9f0aad5f2d3da'',
			''bfefed0549173509cf29947d11c0656825ac024a''
		]
	},
	HiExampleCommit {
		#id : ''e93fc5ac8da85aa1d63b1ad3ecb9f0aad5f2d3da'',
		#comment : ''next label scaling'',
		#parentIds : [
			''d025cd4a1768521a24ae0d79b671782d8ad3c757''
		]
	},
	HiExampleCommit {
		#id : ''d025cd4a1768521a24ae0d79b671782d8ad3c757'',
		#comment : ''remote panel scaling'',
		#parentIds : [
			''67b80880124b85b75730f63930c92d8bf8576c3e''
		]
	},
	HiExampleCommit {
		#id : ''bfefed0549173509cf29947d11c0656825ac024a'',
		#comment : ''Issue #1001\nRecategorize instance-creation -> instance creation'',
		#parentIds : [
			''f83062155670056ac5c0609952dcc9c116492272''
		]
	},
	HiExampleCommit {
		#id : ''67b80880124b85b75730f63930c92d8bf8576c3e'',
		#comment : ''scale defaultActionWidth'',
		#parentIds : [
			''cd411f36068acfa3f5c097355aa3a457aaf030e4''
		]
	},
	HiExampleCommit {
		#id : ''cd411f36068acfa3f5c097355aa3a457aaf030e4'',
		#comment : ''next dimensions with displayScaleFactor'',
		#parentIds : [
			''f527589e04dce75130437b51b2b948d335b11436''
		]
	},
	HiExampleCommit {
		#id : ''f83062155670056ac5c0609952dcc9c116492272'',
		#comment : ''Fix for #999\nReuse existing parser to accept urls like gitolite3@mist-project.org:testing'',
		#parentIds : [
			''a8bc2c8cfe77249075fdb06084afaaa9e5d2e368''
		]
	},
	HiExampleCommit {
		#id : ''f527589e04dce75130437b51b2b948d335b11436'',
		#comment : ''fast table scaling'',
		#parentIds : [
			''c688e9e31dda996dd62e0f7068ab05db409cd92a''
		]
	},
	HiExampleCommit {
		#id : ''c688e9e31dda996dd62e0f7068ab05db409cd92a'',
		#comment : ''next dimensions scaled by displayScaleFactor'',
		#parentIds : [
			''beaa1ad0ab0dcbc9e76bad26f7d5e988a38f2121''
		]
	},
	HiExampleCommit {
		#id : ''beaa1ad0ab0dcbc9e76bad26f7d5e988a38f2121'',
		#comment : ''displayScaleFactor for Spec dimensions'',
		#parentIds : [
			''0201ddb19f2ca66fe19ef65ebb1b843dd1d9d113''
		]
	},
	HiExampleCommit {
		#id : ''0201ddb19f2ca66fe19ef65ebb1b843dd1d9d113'',
		#comment : ''use displayScaleFactor for hardcoded dimensions'',
		#parentIds : [
			''a39ee7f7adf156ee3ee6007decca04c2c45371fb''
		]
	},
	HiExampleCommit {
		#id : ''1d28653affbfda8e942142ca8eed55a580f527b9'',
		#comment : ''getting issue from github'',
		#parentIds : [
			''f9e67f136d8b457e97db26eb7388362634c9af28''
		]
	},
	HiExampleCommit {
		#id : ''f9e67f136d8b457e97db26eb7388362634c9af28'',
		#comment : ''using correct model\nadd focus order'',
		#parentIds : [
			''d5ec515b50931d2c9c76da6e68cc311aef806584''
		]
	},
	HiExampleCommit {
		#id : ''d5ec515b50931d2c9c76da6e68cc311aef806584'',
		#comment : ''add a remote panel'',
		#parentIds : [
			''fa96b55f06f121d1b1487da0b0389214a3b9a4ca''
		]
	},
	HiExampleCommit {
		#id : ''fa96b55f06f121d1b1487da0b0389214a3b9a4ca'',
		#comment : ''created panels and commands (incomplete)'',
		#parentIds : [
			''a39ee7f7adf156ee3ee6007decca04c2c45371fb''
		]
	},
	HiExampleCommit {
		#id : ''baac813e4aa2c541739808427bf6aa0d6b729f48'',
		#comment : ''ensures a class organisation is always present when converting a package.\n\nFixes #992'',
		#parentIds : [
			''a39ee7f7adf156ee3ee6007decca04c2c45371fb''
		]
	},
	HiExampleCommit {
		#id : ''a8bc2c8cfe77249075fdb06084afaaa9e5d2e368'',
		#comment : ''Merge pull request #987 from guillep/issue/986\n\nissue/986'',
		#parentIds : [
			''9f9af6d27419b9314c70be417e77be3ee6784985'',
			''e350aa679938336ba2022df4d928c860b50c032e''
		]
	},
	HiExampleCommit {
		#id : ''9f9af6d27419b9314c70be417e77be3ee6784985'',
		#comment : ''Merge pull request #981 from tesonep/issue/980\n\nissue/980'',
		#parentIds : [
			''45d8f6f840eb5aaba355047f9cb0e7c1ab88ed68'',
			''5b63fa8583cb64bcebce206802eb4f95f93ee7a7''
		]
	},
	HiExampleCommit {
		#id : ''e350aa679938336ba2022df4d928c860b50c032e'',
		#comment : ''Simplification'',
		#parentIds : [
			''d8c92d32a20855af228092c18be5ee76c6d0b718''
		]
	},
	HiExampleCommit {
		#id : ''45d8f6f840eb5aaba355047f9cb0e7c1ab88ed68'',
		#comment : ''Merge pull request #989 from tesonep/issue/988\n\nissue/988'',
		#parentIds : [
			''34d68a214cf891f20255f0a994db457d1a5ed9e8'',
			''df9ce8b83fbc10c7ea6cb33b706f07ddcf426023''
		]
	},
	HiExampleCommit {
		#id : ''df9ce8b83fbc10c7ea6cb33b706f07ddcf426023'',
		#comment : ''Fixing the description'',
		#parentIds : [
			''cf03dff5f07bd662e943d2af19f150bcc90e4f94''
		]
	},
	HiExampleCommit {
		#id : ''cf03dff5f07bd662e943d2af19f150bcc90e4f94'',
		#comment : ''Making the load of packages in a single operation.\nIt accumulates the changes in a single MCLoader and then loads it.'',
		#parentIds : [
			''34d68a214cf891f20255f0a994db457d1a5ed9e8''
		]
	},
	HiExampleCommit {
		#id : ''d8c92d32a20855af228092c18be5ee76c6d0b718'',
		#comment : ''Remove useless test'',
		#parentIds : [
			''524b87ab9486b2e3fb941d7aa338784f8a23528e''
		]
	},
	HiExampleCommit {
		#id : ''524b87ab9486b2e3fb941d7aa338784f8a23528e'',
		#comment : ''Make it work with any class announcement'',
		#parentIds : [
			''52d939d3257a5e6839b0e0858c53bda7e720d36a''
		]
	},
	HiExampleCommit {
		#id : ''52d939d3257a5e6839b0e0858c53bda7e720d36a'',
		#comment : ''Fix #986\nHandle not only the class package but also the method packages'',
		#parentIds : [
			''a39ee7f7adf156ee3ee6007decca04c2c45371fb''
		]
	},
	HiExampleCommit {
		#id : ''34d68a214cf891f20255f0a994db457d1a5ed9e8'',
		#comment : ''Merge pull request #976 from estebanlm/fix-ask-everytime-conflicting-versions\n\nfix-ask-everytime-conflicting-versions'',
		#parentIds : [
			''1066e8446addcb428d5e0fe8bce76cda55bdc6c4'',
			''30d8d0869ae71bdb69c5fc78c99d8d05e9a591f8''
		]
	},
	HiExampleCommit {
		#id : ''5b63fa8583cb64bcebce206802eb4f95f93ee7a7'',
		#comment : ''Fix #982\nAdding a warning message to the Edit Project window.\nAlso, adding a spec component that extends the read only text morph. '',
		#parentIds : [
			''8bb62e6f10ab28f7eae868d2896b0023e23ac38b''
		]
	},
	HiExampleCommit {
		#id : ''8bb62e6f10ab28f7eae868d2896b0023e23ac38b'',
		#comment : ''Fix #980\n- Improving the API of package detection to use String.\n- Discriminate the packages and other files that look like packages but they are not.'',
		#parentIds : [
			''1066e8446addcb428d5e0fe8bce76cda55bdc6c4''
		]
	},
	HiExampleCommit {
		#id : ''30d8d0869ae71bdb69c5fc78c99d8d05e9a591f8'',
		#comment : ''adding comments'',
		#parentIds : [
			''a03e37488fe88db9dd4847179699d6cb2a8fcbdb''
		]
	},
	HiExampleCommit {
		#id : ''a03e37488fe88db9dd4847179699d6cb2a8fcbdb'',
		#comment : ''add "record choice" strategy. '',
		#parentIds : [
			''097f199b2796d935b9237b7a28688b5d710989f4''
		]
	},
	HiExampleCommit {
		#id : ''a39ee7f7adf156ee3ee6007decca04c2c45371fb'',
		#comment : ''Merge 512dca8ca9db66d512e977588b8a38e9ac9fc68b'',
		#parentIds : [
			''1066e8446addcb428d5e0fe8bce76cda55bdc6c4'',
			''512dca8ca9db66d512e977588b8a38e9ac9fc68b''
		]
	},
	HiExampleCommit {
		#id : ''1066e8446addcb428d5e0fe8bce76cda55bdc6c4'',
		#comment : ''Use libgit 1.5.1'',
		#parentIds : [
			''1a9f1ca1ffa025535acf8b9ae3ca988b8f3fe069''
		]
	},
	HiExampleCommit {
		#id : ''1a9f1ca1ffa025535acf8b9ae3ca988b8f3fe069'',
		#comment : ''Merge pull request #970 from guillep/issue/961\n\nFix configuration of remotes'',
		#parentIds : [
			''ba60c03309aaaf22ba431f26ac1e153fc989bb22'',
			''2d16997177f4e52638bb1a7320b1406ea4ee42ee''
		]
	},
	HiExampleCommit {
		#id : ''512dca8ca9db66d512e977588b8a38e9ac9fc68b'',
		#comment : ''Merge pull request #971 from tesonep/issue/967\n\nissue/967'',
		#parentIds : [
			''7d2cf6abe841e1c4367aaf68944279a4b3fe30cf'',
			''a28abe4e2255af610c8f90e50e952135aa12ba11''
		]
	},
	HiExampleCommit {
		#id : ''a28abe4e2255af610c8f90e50e952135aa12ba11'',
		#comment : ''When producing a merge, sometimes Github produces a merge commit that is not necessary. \nThis merge commit includes two commits that are also included in the chain of ancestors of the head.\n\nThe merge should always create the commit with the two parents.'',
		#parentIds : [
			''d76a69788eb7c66513d448c707ba8b21ea52be4c''
		]
	},
	HiExampleCommit {
		#id : ''2d16997177f4e52638bb1a7320b1406ea4ee42ee'',
		#comment : ''Fix #961\n- configure remotes with the same protocol than the clone\n- remove useless retry'',
		#parentIds : [
			''7927248233f6d40e925b4a9f7b4f13358e5a4920''
		]
	},
	HiExampleCommit {
		#id : ''ba60c03309aaaf22ba431f26ac1e153fc989bb22'',
		#comment : ''Merge pull request #968 from guillep/issue/965\n\nFixes for subdirectory resolution'',
		#parentIds : [
			''d76a69788eb7c66513d448c707ba8b21ea52be4c'',
			''994e06b80b34f7e9e407e0876b80b59fcf7ce5da''
		]
	},
	HiExampleCommit {
		#id : ''7927248233f6d40e925b4a9f7b4f13358e5a4920'',
		#comment : ''Fix #964\n - more explicit title and progress bar for importing local repositories'',
		#parentIds : [
			''994e06b80b34f7e9e407e0876b80b59fcf7ce5da''
		]
	},
	HiExampleCommit {
		#id : ''994e06b80b34f7e9e407e0876b80b59fcf7ce5da'',
		#comment : ''Fix #965\nFix path resolution for Pharo 6.1'',
		#parentIds : [
			''3432004573c170ba5b42a0c0fb8a26937dddc1f4''
		]
	},
	HiExampleCommit {
		#id : ''3432004573c170ba5b42a0c0fb8a26937dddc1f4'',
		#comment : ''Fix #965\n - added case for empty detect'',
		#parentIds : [
			''ec3f43c034fcf68c14e1b8e5d64803c96b092964''
		]
	},
	HiExampleCommit {
		#id : ''ec3f43c034fcf68c14e1b8e5d64803c96b092964'',
		#comment : ''Fix #965'',
		#parentIds : [
			''11f15125116ad32b43cfb83a4c3f276adace3916''
		]
	},
	HiExampleCommit {
		#id : ''11f15125116ad32b43cfb83a4c3f276adace3916'',
		#comment : ''Added test case for complex subdirectory'',
		#parentIds : [
			''097f199b2796d935b9237b7a28688b5d710989f4''
		]
	},
	HiExampleCommit {
		#id : ''7d2cf6abe841e1c4367aaf68944279a4b3fe30cf'',
		#comment : ''Merge pull request #960 from SergeStinckwich/patch-2\n\nFix the FAQ'',
		#parentIds : [
			''097f199b2796d935b9237b7a28688b5d710989f4'',
			''6387f80b8a8973fcc809815859fbe87fccb1dd55''
		]
	},
	HiExampleCommit {
		#id : ''d76a69788eb7c66513d448c707ba8b21ea52be4c'',
		#comment : ''Merge pull request #963 from estebanlm/fix-gitlocal-without-project\n\nfix-gitlocal-without-project'',
		#parentIds : [
			''097f199b2796d935b9237b7a28688b5d710989f4'',
			''30f6aab839fb8e10a77186ee0b23a1114d304f2e''
		]
	},
	HiExampleCommit {
		#id : ''30f6aab839fb8e10a77186ee0b23a1114d304f2e'',
		#comment : ''fixes #962'',
		#parentIds : [
			''097f199b2796d935b9237b7a28688b5d710989f4''
		]
	},
	HiExampleCommit {
		#id : ''6387f80b8a8973fcc809815859fbe87fccb1dd55'',
		#comment : ''Update README.md\n\nFix the FAQ'',
		#parentIds : [
			''097f199b2796d935b9237b7a28688b5d710989f4''
		]
	},
	HiExampleCommit {
		#id : ''097f199b2796d935b9237b7a28688b5d710989f4'',
		#comment : ''Merge pull request #958 from tesonep/issue/950\n\nissue/950'',
		#parentIds : [
			''ecec3589d1db81e3ecf8b2d65d5260d82c181f00'',
			''35922fc9a7146caaa92a9024d89a512d976c7f1c''
		]
	},
	HiExampleCommit {
		#id : ''ecec3589d1db81e3ecf8b2d65d5260d82c181f00'',
		#comment : ''Merge pull request #957 from guillep/issue/951\n\nRename directory label into "code directory"'',
		#parentIds : [
			''aec5251d2aaab4b773793c6c90d3c8fe7dfd0c76'',
			''f8cdbf417ada1bc47cff8598b5a879b4f68b1635''
		]
	},
	HiExampleCommit {
		#id : ''35922fc9a7146caaa92a9024d89a512d976c7f1c'',
		#comment : ''FIx #950\nWhen a properties is created it should have the default value.'',
		#parentIds : [
			''aec5251d2aaab4b773793c6c90d3c8fe7dfd0c76''
		]
	},
	HiExampleCommit {
		#id : ''f8cdbf417ada1bc47cff8598b5a879b4f68b1635'',
		#comment : ''Rename directory into code directory'',
		#parentIds : [
			''287e5104b69c7edc054a941a64e8cf4551dffef8''
		]
	},
	HiExampleCommit {
		#id : ''aec5251d2aaab4b773793c6c90d3c8fe7dfd0c76'',
		#comment : ''Merge pull request #956 from guillep/issue/952\n\nSeveral improvements in Pharo integration'',
		#parentIds : [
			''7ca31853185d007f63e2cd0ae67d1cb8f9fd15b9'',
			''cb99def727abec462cd97cc0a0db87c3dea6f268''
		]
	},
	HiExampleCommit {
		#id : ''cb99def727abec462cd97cc0a0db87c3dea6f268'',
		#comment : ''Remove need of ston file\n'',
		#parentIds : [
			''287e5104b69c7edc054a941a64e8cf4551dffef8''
		]
	},
	HiExampleCommit {
		#id : ''287e5104b69c7edc054a941a64e8cf4551dffef8'',
		#comment : ''Be able to do github anonymous requests'',
		#parentIds : [
			''af774af8257d0c53cdb69e408c8f87f3094cd3ce''
		]
	},
	HiExampleCommit {
		#id : ''af774af8257d0c53cdb69e408c8f87f3094cd3ce'',
		#comment : ''Better conditions for repair actions'',
		#parentIds : [
			''590c03cec6464eb2c9286fb6787c154f057918c2''
		]
	},
	HiExampleCommit {
		#id : ''590c03cec6464eb2c9286fb6787c154f057918c2'',
		#comment : ''Fix for #952\n - test adoption of unknown commit\n - add implementation of adopt to unknown commit'',
		#parentIds : [
			''1063ab202ace18c019e94a970d67e0a871348460''
		]
	},
	HiExampleCommit {
		#id : ''1063ab202ace18c019e94a970d67e0a871348460'',
		#comment : ''Merge 7ca31853185d007f63e2cd0ae67d1cb8f9fd15b9'',
		#parentIds : [
			''f305a58f1a7d78f63e76039899276e9c785cc3d6'',
			''7ca31853185d007f63e2cd0ae67d1cb8f9fd15b9''
		]
	},
	HiExampleCommit {
		#id : ''7ca31853185d007f63e2cd0ae67d1cb8f9fd15b9'',
		#comment : ''Merge pull request #943 from tesonep/issue/940\n\nFix #940 Installing new Iceberg should bootstrap Pharo'',
		#parentIds : [
			''0aa9f180c43a828c18083da0029f914b82acad74'',
			''9d47ae4ef3e496b46fed16290f461d204fdda22e''
		]
	},
	HiExampleCommit {
		#id : ''f305a58f1a7d78f63e76039899276e9c785cc3d6'',
		#comment : ''Merge branch \''dev-1.0\'' of github.com:pharo-vcs/iceberg into dev-1.0\n'',
		#parentIds : [
			''910479351e3e626d0761f06cfa67c472ea8fbe54'',
			''0aa9f180c43a828c18083da0029f914b82acad74''
		]
	},
	HiExampleCommit {
		#id : ''9d47ae4ef3e496b46fed16290f461d204fdda22e'',
		#comment : ''Installing correclty Pharo Repository\n'',
		#parentIds : [
			''4347d8cab0b24969c0b70ca8001281c11e1688a4''
		]
	},
	HiExampleCommit {
		#id : ''4347d8cab0b24969c0b70ca8001281c11e1688a4'',
		#comment : ''Installing correclty Pharo Repository\n'',
		#parentIds : [
			''785dacc479cdc5877749af6968f0096ab63c3a08''
		]
	},
	HiExampleCommit {
		#id : ''910479351e3e626d0761f06cfa67c472ea8fbe54'',
		#comment : ''Merge branch \''master\'' of github.com:pharo-vcs/iceberg into dev-1.0\n\nMerged .travis.yml to manage sync of wiki and update iceberg in pharo job\n'',
		#parentIds : [
			''05e6d5adfafed217188b33d0e6735002fa885a3f'',
			''2ae594a7bcd5922b3620fbf56c0c1784bd654eeb''
		]
	},
	HiExampleCommit {
		#id : ''785dacc479cdc5877749af6968f0096ab63c3a08'',
		#comment : ''Installing correclty Pharo Repository\n'',
		#parentIds : [
			''1b7861bc2f32bd874f3672169424ba560c5181a6''
		]
	},
	HiExampleCommit {
		#id : ''1b7861bc2f32bd874f3672169424ba560c5181a6'',
		#comment : ''Merge branch \''dev-1.0\'' into issue/940\n'',
		#parentIds : [
			''acadcd02f64515766e154f13dc8de7d67f45a225'',
			''0aa9f180c43a828c18083da0029f914b82acad74''
		]
	},
	HiExampleCommit {
		#id : ''0aa9f180c43a828c18083da0029f914b82acad74'',
		#comment : ''Merge pull request #947 from tesonep/fixing-tests\n\nFixing tests'',
		#parentIds : [
			''cc486bc2cc14617934ec0f9c4a2b1f60d5989489'',
			''e21cfd2d2439ef8125ccd4eb0d4e873f8b2ef731''
		]
	},
	HiExampleCommit {
		#id : ''05e6d5adfafed217188b33d0e6735002fa885a3f'',
		#comment : ''Fix #948\nUse SSH url instead of Git URL'',
		#parentIds : [
			''01ab7fb67958a9f754a7f2e7118aa0c7b782c93d''
		]
	},
	HiExampleCommit {
		#id : ''e21cfd2d2439ef8125ccd4eb0d4e873f8b2ef731'',
		#comment : ''Fixing the Metacello Integration Tests.'',
		#parentIds : [
			''852ca13d8a7a926b538fd47b038993e19c7f7473''
		]
	},
	HiExampleCommit {
		#id : ''cc486bc2cc14617934ec0f9c4a2b1f60d5989489'',
		#comment : ''Fix #941'',
		#parentIds : [
			''6f02ee9f01d8b2001d3389db18afcd6c819a0444''
		]
	},
	HiExampleCommit {
		#id : ''6f02ee9f01d8b2001d3389db18afcd6c819a0444'',
		#comment : ''Move pharo repository bootstrap to Pharo plugin'',
		#parentIds : [
			''852ca13d8a7a926b538fd47b038993e19c7f7473''
		]
	},
	HiExampleCommit {
		#id : ''acadcd02f64515766e154f13dc8de7d67f45a225'',
		#comment : ''Fix #940 Installing new Iceberg should bootstrap Pharo\n'',
		#parentIds : [
			''852ca13d8a7a926b538fd47b038993e19c7f7473''
		]
	},
	HiExampleCommit {
		#id : ''852ca13d8a7a926b538fd47b038993e19c7f7473'',
		#comment : ''Pass on migration tests'',
		#parentIds : [
			''84bd0a9552ce972117da4e8110bae1401c1a72f2''
		]
	},
	HiExampleCommit {
		#id : ''84bd0a9552ce972117da4e8110bae1401c1a72f2'',
		#comment : ''Automatic rewrite of OSEnvironment default => OSEnvironment current'',
		#parentIds : [
			''9e521544d3735063c2e2eae3c9c0fd5396e82956''
		]
	},
	HiExampleCommit {
		#id : ''9e521544d3735063c2e2eae3c9c0fd5396e82956'',
		#comment : ''There is no need to add packages.'',
		#parentIds : [
			''162cb54135feb8d013f23a0cc1ebf8b999e1a499''
		]
	},
	HiExampleCommit {
		#id : ''162cb54135feb8d013f23a0cc1ebf8b999e1a499'',
		#comment : ''Only set project if read project is valid'',
		#parentIds : [
			''cbb836a4f4cac1ee777c065346fafbee8ea0eba4''
		]
	},
	HiExampleCommit {
		#id : ''cbb836a4f4cac1ee777c065346fafbee8ea0eba4'',
		#comment : ''Fix #932\nFix update pharo repository in case of locating already cloned repository'',
		#parentIds : [
			''1dee75f9be7a641f94fc9725cd77ffa41e46c2de''
		]
	},
	HiExampleCommit {
		#id : ''1dee75f9be7a641f94fc9725cd77ffa41e46c2de'',
		#comment : ''Access the handle with the right accessor'',
		#parentIds : [
			''bacd81630d1d0bbcee2f9ed72114e610c32c816e''
		]
	},
	HiExampleCommit {
		#id : ''bacd81630d1d0bbcee2f9ed72114e610c32c816e'',
		#comment : ''Correctly initialize the format list'',
		#parentIds : [
			''4248295ff7f6e334b1139b41864a3adaf229c4e2''
		]
	},
	HiExampleCommit {
		#id : ''4248295ff7f6e334b1139b41864a3adaf229c4e2'',
		#comment : ''Add execution permissions to test script\n'',
		#parentIds : [
			''01ab7fb67958a9f754a7f2e7118aa0c7b782c93d''
		]
	},
	HiExampleCommit {
		#id : ''01ab7fb67958a9f754a7f2e7118aa0c7b782c93d'',
		#comment : ''Fix #939\n\nRemove duplicated IceInteractiveErrorVisitor'',
		#parentIds : [
			''729e76260b55aa3a2b92f3bd148859fcf7267c5b''
		]
	},
	HiExampleCommit {
		#id : ''729e76260b55aa3a2b92f3bd148859fcf7267c5b'',
		#comment : ''No commits should behave like a commit'',
		#parentIds : [
			''1681524044a8444dad0e7051e337338170b9c3c5''
		]
	},
	HiExampleCommit {
		#id : ''1681524044a8444dad0e7051e337338170b9c3c5'',
		#comment : ''Fix #938\n\nDo not catch AssertionFailure because it masks errors.'',
		#parentIds : [
			''cc94fdc7f773b3d926033fd3a5de4367bb6ae69f''
		]
	},
	HiExampleCommit {
		#id : ''cc94fdc7f773b3d926033fd3a5de4367bb6ae69f'',
		#comment : ''Fix #934\nUse repository directory in gitlocal directory in metacello instead of package directory'',
		#parentIds : [
			''41d4b974274c15898490c201618b38eac8bb8033''
		]
	},
	HiExampleCommit {
		#id : ''41d4b974274c15898490c201618b38eac8bb8033'',
		#comment : ''Merge ffdfbb4a770efde8ebb172c6b80687929090dc5c'',
		#parentIds : [
			''303140189f1690e7cd71d02d1c398d4a8622b766'',
			''ffdfbb4a770efde8ebb172c6b80687929090dc5c''
		]
	},
	HiExampleCommit {
		#id : ''303140189f1690e7cd71d02d1c398d4a8622b766'',
		#comment : ''add #forceCalculateDirtyPackages'',
		#parentIds : [
			''9450718881aeeaff0ac844d5bb77bffa285ddeca''
		]
	},
	HiExampleCommit {
		#id : ''ffdfbb4a770efde8ebb172c6b80687929090dc5c'',
		#comment : ''When a repository is created from metacello with source directory, the properties file should be read from the correct subdirectory.\nIt should be refreshed after setting the new project. '',
		#parentIds : [
			''0c33463dec1fface7df68ad2776c2faed9f107bd''
		]
	},
	HiExampleCommit {
		#id : ''0c33463dec1fface7df68ad2776c2faed9f107bd'',
		#comment : ''Merge pull request #936 from guillep/dev-1.0\n\ndev-1.0'',
		#parentIds : [
			''47e002896d047c6622f25f79fc62cd1439b16797'',
			''9450718881aeeaff0ac844d5bb77bffa285ddeca''
		]
	},
	HiExampleCommit {
		#id : ''47e002896d047c6622f25f79fc62cd1439b16797'',
		#comment : ''Fixing the smalltalkCI to run only the required tests and only once\n'',
		#parentIds : [
			''69585b0610898cb70930f2a1c5c4dba0197779ea''
		]
	},
	HiExampleCommit {
		#id : ''9450718881aeeaff0ac844d5bb77bffa285ddeca'',
		#comment : ''Add #933\n\nSimplify Edit project dialog:\n - add root directory\n - handle deselection (and select root)'',
		#parentIds : [
			''69585b0610898cb70930f2a1c5c4dba0197779ea''
		]
	},
	HiExampleCommit {
		#id : ''69585b0610898cb70930f2a1c5c4dba0197779ea'',
		#comment : ''Do not use proportional layout in trees'',
		#parentIds : [
			''f3ada609d96c40939f5a0e762a59a567358e5fc2''
		]
	},
	HiExampleCommit {
		#id : ''2ae594a7bcd5922b3620fbf56c0c1784bd654eeb'',
		#comment : ''Merge 3ca4ebb1f92ff16f10ec9bce3a20f49053abe9f2'',
		#parentIds : [
			''c55282096ed0e258c18b810a555751e016ad281c'',
			''3ca4ebb1f92ff16f10ec9bce3a20f49053abe9f2''
		]
	},
	HiExampleCommit {
		#id : ''3ca4ebb1f92ff16f10ec9bce3a20f49053abe9f2'',
		#comment : ''Upgrade commander to 0.6.3'',
		#parentIds : [
			''726254360c987b9b3472a6098b5869970e819cc9''
		]
	},
	HiExampleCommit {
		#id : ''f3ada609d96c40939f5a0e762a59a567358e5fc2'',
		#comment : ''Move package commit cache to superclass'',
		#parentIds : [
			''2a6b6f412a84553e18e550b59860dd6bc716b50b''
		]
	},
	HiExampleCommit {
		#id : ''2a6b6f412a84553e18e550b59860dd6bc716b50b'',
		#comment : ''Merge pull request #929 from tesonep/fixingInstallationOfBaselines\n\nfixingInstallationOfBaselines'',
		#parentIds : [
			''d919ae9ca3083351469a891a8783848e60204d63'',
			''4cb8427af98d18d19266a313fadbecbf6f8aa11c''
		]
	},
	HiExampleCommit {
		#id : ''4cb8427af98d18d19266a313fadbecbf6f8aa11c'',
		#comment : ''Fixing the installation of baselines.'',
		#parentIds : [
			''d919ae9ca3083351469a891a8783848e60204d63''
		]
	},
	HiExampleCommit {
		#id : ''d919ae9ca3083351469a891a8783848e60204d63'',
		#comment : ''Better condition to discard changes'',
		#parentIds : [
			''9dbf3b77c1dd829986a1583d40fac992691926fa''
		]
	},
	HiExampleCommit {
		#id : ''9dbf3b77c1dd829986a1583d40fac992691926fa'',
		#comment : ''Externalize cache of package per commit\n - this allows to invalidate the cache when the project is modified (useful to support old invalid projects)'',
		#parentIds : [
			''8d1487482853d572ec8c43b1e13d7350d334bbf8''
		]
	},
	HiExampleCommit {
		#id : ''8d1487482853d572ec8c43b1e13d7350d334bbf8'',
		#comment : ''Merge pull request #925 from guillep/feature/project-hooks\n\nfeature/project-hooks'',
		#parentIds : [
			''6ec8dc7f281aac45270aef951dc15c91447fe0b8'',
			''d086b3a0cf084bca1d7a3fae490cebb6c3e7150a''
		]
	},
	HiExampleCommit {
		#id : ''c55282096ed0e258c18b810a555751e016ad281c'',
		#comment : ''Merge pull request #926 from SergeStinckwich/patch-1\n\nAdd one entry in the FAQ about 2FA'',
		#parentIds : [
			''11b322d472782677236ca9d482a6fa6c259e3128'',
			''0dcf45a5c176662cdd02f2e5c309e8eff6e0dc0a''
		]
	},
	HiExampleCommit {
		#id : ''11b322d472782677236ca9d482a6fa6c259e3128'',
		#comment : ''Merge pull request #922 from seandenigris/seandenigris-patch-1\n\nCookbook: Add "Changing Last Commit Author"'',
		#parentIds : [
			''47161073b17161aefc5aa8693c3f928c0c8216f2'',
			''1e515c695f933caafc49995f9b4e29d1ce7464c7''
		]
	},
	HiExampleCommit {
		#id : ''0dcf45a5c176662cdd02f2e5c309e8eff6e0dc0a'',
		#comment : ''Update README.md'',
		#parentIds : [
			''47161073b17161aefc5aa8693c3f928c0c8216f2''
		]
	},
	HiExampleCommit {
		#id : ''d086b3a0cf084bca1d7a3fae490cebb6c3e7150a'',
		#comment : ''Check correct path from fileSystem'',
		#parentIds : [
			''a815318580a2497edfecda00479f113525fb3e0e''
		]
	},
	HiExampleCommit {
		#id : ''a815318580a2497edfecda00479f113525fb3e0e'',
		#comment : ''Fix tests'',
		#parentIds : [
			''dd6b3ccf67c936093c6d0ac100e03c668e552826''
		]
	},
	HiExampleCommit {
		#id : ''dd6b3ccf67c936093c6d0ac100e03c668e552826'',
		#comment : ''Merge pull request #2 from demarey/fix/metacello-loading\n\nfix/metacello-loading'',
		#parentIds : [
			''45ff6c471ae08d4d1606797c6971571335eced68'',
			''fd2f6736e1aa01545070fe0fb10425dc720760e9''
		]
	},
	HiExampleCommit {
		#id : ''fd2f6736e1aa01545070fe0fb10425dc720760e9'',
		#comment : ''A repository properties should know its own commitish'',
		#parentIds : [
			''45ff6c471ae08d4d1606797c6971571335eced68''
		]
	},
	HiExampleCommit {
		#id : ''45ff6c471ae08d4d1606797c6971571335eced68'',
		#comment : ''Merge 64139beecdc3271f1ce44b52da56c2a0623c5223'',
		#parentIds : [
			''7cac3748da5adc312db11ee539f7519d9421c2e2'',
			''64139beecdc3271f1ce44b52da56c2a0623c5223''
		]
	},
	HiExampleCommit {
		#id : ''7cac3748da5adc312db11ee539f7519d9421c2e2'',
		#comment : ''Fix metacello installation plugin'',
		#parentIds : [
			''1787eb59639dc2ffb47281bd80f57553580f300d''
		]
	},
	HiExampleCommit {
		#id : ''64139beecdc3271f1ce44b52da56c2a0623c5223'',
		#comment : ''Merge pull request #1 from demarey/dev-1.0\n\nfix pb in repair metadata action'',
		#parentIds : [
			''1787eb59639dc2ffb47281bd80f57553580f300d'',
			''edf727fe70721bd6f17e985851ef697b422904f8''
		]
	},
	HiExampleCommit {
		#id : ''edf727fe70721bd6f17e985851ef697b422904f8'',
		#comment : ''fix pbs in repair metadata action'',
		#parentIds : [
			''8472b152060fb37fb2b5a59f5cf42773c4e11877''
		]
	},
	HiExampleCommit {
		#id : ''8472b152060fb37fb2b5a59f5cf42773c4e11877'',
		#comment : ''Merge 1787eb59639dc2ffb47281bd80f57553580f300d'',
		#parentIds : [
			''6ec8dc7f281aac45270aef951dc15c91447fe0b8'',
			''1787eb59639dc2ffb47281bd80f57553580f300d''
		]
	},
	HiExampleCommit {
		#id : ''1787eb59639dc2ffb47281bd80f57553580f300d'',
		#comment : ''Tests are passing'',
		#parentIds : [
			''6745482ef0ab268d417dbb65f8f3608c3036bbc3''
		]
	},
	HiExampleCommit {
		#id : ''6745482ef0ab268d417dbb65f8f3608c3036bbc3'',
		#comment : ''Fixing some tests using the wrong API'',
		#parentIds : [
			''349a4b4d1e50762038216922c79976edb7870340''
		]
	},
	HiExampleCommit {
		#id : ''349a4b4d1e50762038216922c79976edb7870340'',
		#comment : ''Remove obsolete methods'',
		#parentIds : [
			''ea3af2f97f94825602b5d1e9c78477f3d675ad10''
		]
	},
	HiExampleCommit {
		#id : ''ea3af2f97f94825602b5d1e9c78477f3d675ad10'',
		#comment : ''Working tests'',
		#parentIds : [
			''27634978ba54e5a2c3d069787a4714eebe220bd8''
		]
	},
	HiExampleCommit {
		#id : ''27634978ba54e5a2c3d069787a4714eebe220bd8'',
		#comment : ''Getting commitish properties from commitish itself instead of project'',
		#parentIds : [
			''3d764ea44a1e534980fc4a1acd4bddfdb0164b83''
		]
	},
	HiExampleCommit {
		#id : ''3d764ea44a1e534980fc4a1acd4bddfdb0164b83'',
		#comment : ''Split unborn project from basic project'',
		#parentIds : [
			''c3a67f5bef37ee9017580acfae043ec233ffabb9''
		]
	},
	HiExampleCommit {
		#id : ''c3a67f5bef37ee9017580acfae043ec233ffabb9'',
		#comment : ''correctly detecting dirty projects of missing repositories'',
		#parentIds : [
			''f91e232395bf537e1389b361d4af19f4e61ccb79''
		]
	},
	HiExampleCommit {
		#id : ''f91e232395bf537e1389b361d4af19f4e61ccb79'',
		#comment : ''Resolve packages with path'',
		#parentIds : [
			''5542f06b35fe15aed781521df65736e4f160430e''
		]
	},
	HiExampleCommit {
		#id : ''5542f06b35fe15aed781521df65736e4f160430e'',
		#comment : ''Refactoring projects'',
		#parentIds : [
			''2eab0363f36437ce2ecdb90f9cd572d131300d6b''
		]
	},
	HiExampleCommit {
		#id : ''2eab0363f36437ce2ecdb90f9cd572d131300d6b'',
		#comment : ''Make project readers prioritizable'',
		#parentIds : [
			''a0144320aca77ffd7bda7b584cd6898e30470fb4''
		]
	},
	HiExampleCommit {
		#id : ''a0144320aca77ffd7bda7b584cd6898e30470fb4'',
		#comment : ''- Delegate reading project to project reader\n- Delegate writing to project object'',
		#parentIds : [
			''795aeb8a3fe20c7a41c7736ab9838adbf9421853''
		]
	},
	HiExampleCommit {
		#id : ''6ec8dc7f281aac45270aef951dc15c91447fe0b8'',
		#comment : ''Merge pull request #921 from tesonep/feature/testingUpdate\n\nAdding a Job to travis to test the ability of updating iceberg in a Pharo7'',
		#parentIds : [
			''795aeb8a3fe20c7a41c7736ab9838adbf9421853'',
			''dc7b5cbc0f0547d89d7884f81e008e9ca2d0efe0''
		]
	},
	HiExampleCommit {
		#id : ''1e515c695f933caafc49995f9b4e29d1ce7464c7'',
		#comment : ''Cookbook: Add "Changing Last Commit Author"'',
		#parentIds : [
			''47161073b17161aefc5aa8693c3f928c0c8216f2''
		]
	},
	HiExampleCommit {
		#id : ''795aeb8a3fe20c7a41c7736ab9838adbf9421853'',
		#comment : ''Merge 001cce5d51ee73ee2c7dfbc129246e427006809a'',
		#parentIds : [
			''3c9959e83ba4482a907d2aa569d8e4a375e67c5d'',
			''001cce5d51ee73ee2c7dfbc129246e427006809a''
		]
	},
	HiExampleCommit {
		#id : ''3c9959e83ba4482a907d2aa569d8e4a375e67c5d'',
		#comment : ''Accessing the project name'',
		#parentIds : [
			''41baefd7869192759e65975082d04cdad9b7813c''
		]
	},
	HiExampleCommit {
		#id : ''dc7b5cbc0f0547d89d7884f81e008e9ca2d0efe0'',
		#comment : ''Fixing never ending script, and documenting the custom shell\n'',
		#parentIds : [
			''1102841e060d210095ff60971aaba536dd01e3d3''
		]
	},
	HiExampleCommit {
		#id : ''1102841e060d210095ff60971aaba536dd01e3d3'',
		#comment : ''Adding Testing of the update of iceberg\n'',
		#parentIds : [
			''dff50fb04728dac4cc742569ebefd80721b85db4''
		]
	},
	HiExampleCommit {
		#id : ''dff50fb04728dac4cc742569ebefd80721b85db4'',
		#comment : ''Adding Testing of the update of iceberg\n'',
		#parentIds : [
			''3618910648c9855485213004000b41ccf12b6155''
		]
	},
	HiExampleCommit {
		#id : ''3618910648c9855485213004000b41ccf12b6155'',
		#comment : ''Fixing the travis file\n'',
		#parentIds : [
			''9640b2d8cd54b44bee01e6ec83de1488d386aea2''
		]
	},
	HiExampleCommit {
		#id : ''9640b2d8cd54b44bee01e6ec83de1488d386aea2'',
		#comment : ''Adding empty ston for updates\n'',
		#parentIds : [
			''182eeccbb7d39c82b58604fc3b2d6272c049d55b''
		]
	},
	HiExampleCommit {
		#id : ''182eeccbb7d39c82b58604fc3b2d6272c049d55b'',
		#comment : ''Adding another job for testing the update\n'',
		#parentIds : [
			''81fb09b7110fbca634a15a10704618d651fbe90c''
		]
	},
	HiExampleCommit {
		#id : ''81fb09b7110fbca634a15a10704618d651fbe90c'',
		#comment : ''Adding another job for testing the update\n'',
		#parentIds : [
			''197d34488c7271c2ba2cd2fcfebe94b75b601f34''
		]
	},
	HiExampleCommit {
		#id : ''197d34488c7271c2ba2cd2fcfebe94b75b601f34'',
		#comment : ''Adding conditional run\n'',
		#parentIds : [
			''001cce5d51ee73ee2c7dfbc129246e427006809a''
		]
	},
	HiExampleCommit {
		#id : ''001cce5d51ee73ee2c7dfbc129246e427006809a'',
		#comment : ''Fixing the postload of iceberg.'',
		#parentIds : [
			''41baefd7869192759e65975082d04cdad9b7813c''
		]
	},
	HiExampleCommit {
		#id : ''47161073b17161aefc5aa8693c3f928c0c8216f2'',
		#comment : ''Change encrypted key\n'',
		#parentIds : [
			''e6f275d5381c0eeb3c6836db18dbafa9d66fa127''
		]
	},
	HiExampleCommit {
		#id : ''e6f275d5381c0eeb3c6836db18dbafa9d66fa127'',
		#comment : ''Merge pull request #915 from guillep/issue/914\n\nAdd job to sync wiki'',
		#parentIds : [
			''e1267a3fb483b7a4d32c8e2fafee4b089477e820'',
			''f6490fa903f3fa8fdc87b647e78406613a1e4aee''
		]
	},
	HiExampleCommit {
		#id : ''f6490fa903f3fa8fdc87b647e78406613a1e4aee'',
		#comment : ''Give push rights with GH_token\n'',
		#parentIds : [
			''1d231a52673526ddbc573b6cd4474d33f12ff3cc''
		]
	},
	HiExampleCommit {
		#id : ''1d231a52673526ddbc573b6cd4474d33f12ff3cc'',
		#comment : ''Correctly clean and copy wiki repository\n'',
		#parentIds : [
			''167b00044a09b3c8b84fe7304252072ff050069f''
		]
	},
	HiExampleCommit {
		#id : ''167b00044a09b3c8b84fe7304252072ff050069f'',
		#comment : ''Better Sync-wiki\n'',
		#parentIds : [
			''408e958fb9939a8b8fd26accd306f8357a1d3fee''
		]
	},
	HiExampleCommit {
		#id : ''408e958fb9939a8b8fd26accd306f8357a1d3fee'',
		#comment : ''Put docs up to date\n'',
		#parentIds : [
			''1f511797780cb79c0360256590b082f4a0e16c76''
		]
	},
	HiExampleCommit {
		#id : ''1f511797780cb79c0360256590b082f4a0e16c76'',
		#comment : ''Run sync only if PR = false\n'',
		#parentIds : [
			''9ead486b524e33eb6bcd223667adcfb2a7c7a9cf''
		]
	},
	HiExampleCommit {
		#id : ''9ead486b524e33eb6bcd223667adcfb2a7c7a9cf'',
		#comment : ''Fix bash condition\n'',
		#parentIds : [
			''f13ad7437e655cf9689db1d35c4577a975c6315a''
		]
	},
	HiExampleCommit {
		#id : ''f13ad7437e655cf9689db1d35c4577a975c6315a'',
		#comment : ''use correct smalltalk keys\n'',
		#parentIds : [
			''6e756ef9d3f8451f5e53b7287f175fb200e6ab46''
		]
	},
	HiExampleCommit {
		#id : ''6e756ef9d3f8451f5e53b7287f175fb200e6ab46'',
		#comment : ''Exclude all sync-travis jobs but one\n'',
		#parentIds : [
			''50d01cd5125ec733d723212e05aa2cdd6414773c''
		]
	},
	HiExampleCommit {
		#id : ''50d01cd5125ec733d723212e05aa2cdd6414773c'',
		#comment : ''push to wiki only in master branch\n'',
		#parentIds : [
			''366cb0fbf0bc9402b6a78b433507593ff68f833f''
		]
	},
	HiExampleCommit {
		#id : ''366cb0fbf0bc9402b6a78b433507593ff68f833f'',
		#comment : ''Add travis Job to sync wiki\nFixes #914\n'',
		#parentIds : [
			''e1267a3fb483b7a4d32c8e2fafee4b089477e820''
		]
	},
	HiExampleCommit {
		#id : ''e1267a3fb483b7a4d32c8e2fafee4b089477e820'',
		#comment : ''Merge pull request #909 from macta/move-githubwiki-to-docs\n\nMove wiki files to a docs directory (so we can accept pull requests) â\u0080¦'',
		#parentIds : [
			''726254360c987b9b3472a6098b5869970e819cc9'',
			''e7b2837434d4e6164b389230d15990ff592abf29''
		]
	},
	HiExampleCommit {
		#id : ''41baefd7869192759e65975082d04cdad9b7813c'',
		#comment : ''Fix #911\nShow repair checkout branch option if project is unborn'',
		#parentIds : [
			''c5b7cd3bec10e60934d4861841fee3f20c95e46e''
		]
	},
	HiExampleCommit {
		#id : ''e7b2837434d4e6164b389230d15990ff592abf29'',
		#comment : ''Move wiki files to a docs directory (so we can accept pull requests) along with some minor grammatical changes and a new entry for recovering code.\n'',
		#parentIds : [
			''726254360c987b9b3472a6098b5869970e819cc9''
		]
	},
	HiExampleCommit {
		#id : ''c5b7cd3bec10e60934d4861841fee3f20c95e46e'',
		#comment : ''Fix #869'',
		#parentIds : [
			''5d9e433f14b2b025700d5b752edf0cd9bde9cf31''
		]
	},
	HiExampleCommit {
		#id : ''5d9e433f14b2b025700d5b752edf0cd9bde9cf31'',
		#comment : ''Fix #875'',
		#parentIds : [
			''f228f79d964929c771897e12923ae053e1e8ad15''
		]
	},
	HiExampleCommit {
		#id : ''f228f79d964929c771897e12923ae053e1e8ad15'',
		#comment : ''Add test for #875'',
		#parentIds : [
			''4a811fb4427bbdf1f69e9d085a33d52abcfc8927''
		]
	},
	HiExampleCommit {
		#id : ''4a811fb4427bbdf1f69e9d085a33d52abcfc8927'',
		#comment : ''Fix #828'',
		#parentIds : [
			''facf4df4fbcf4d6f8d1d890d09132cb7a2b15de8''
		]
	},
	HiExampleCommit {
		#id : ''facf4df4fbcf4d6f8d1d890d09132cb7a2b15de8'',
		#comment : ''Fix #901\n - move #projectName to its superclass'',
		#parentIds : [
			''834375e2d0110f561b84540b19d2752129f5c31a''
		]
	},
	HiExampleCommit {
		#id : ''834375e2d0110f561b84540b19d2752129f5c31a'',
		#comment : ''Simplify management of source directory paths'',
		#parentIds : [
			''226aa6144961c3c4d42c7a13fba51356620ba88d''
		]
	},
	HiExampleCommit {
		#id : ''226aa6144961c3c4d42c7a13fba51356620ba88d'',
		#comment : ''Add tests to enforce positioning of toolbar buttons'',
		#parentIds : [
			''ad42789986616f81e608f911f175b431295442b4''
		]
	},
	HiExampleCommit {
		#id : ''ad42789986616f81e608f911f175b431295442b4'',
		#comment : ''Fix location of add repository test'',
		#parentIds : [
			''d22080be6542853b8381dc839265572fd5b58100''
		]
	},
	HiExampleCommit {
		#id : ''d22080be6542853b8381dc839265572fd5b58100'',
		#comment : ''Fix project equality'',
		#parentIds : [
			''4d71cef52e41dd2615c0b83d7382194c0524700d''
		]
	},
	HiExampleCommit {
		#id : ''4d71cef52e41dd2615c0b83d7382194c0524700d'',
		#comment : ''A born project should never have an unborn properties'',
		#parentIds : [
			''320308ab634cf57eb35393dca6e66ff45b0f507b''
		]
	},
	HiExampleCommit {
		#id : ''320308ab634cf57eb35393dca6e66ff45b0f507b'',
		#comment : ''Fix memory tests'',
		#parentIds : [
			''a79e07d765dbf4bd0ac35ac6a8639765b06a37ac''
		]
	},
	HiExampleCommit {
		#id : ''a79e07d765dbf4bd0ac35ac6a8639765b06a37ac'',
		#comment : ''Add support for unborn repositories'',
		#parentIds : [
			''993cc626a96257ec09765ee7007dbcb7176c9cd2''
		]
	},
	HiExampleCommit {
		#id : ''993cc626a96257ec09765ee7007dbcb7176c9cd2'',
		#comment : ''Only unload loaded packages'',
		#parentIds : [
			''b77d9652a7231121ec1d7900c73a8b94bd3c16c0''
		]
	},
	HiExampleCommit {
		#id : ''b77d9652a7231121ec1d7900c73a8b94bd3c16c0'',
		#comment : ''Merge pull request #904 from pharo-vcs/enh/metacello\n\nAdd more metacello integration tests'',
		#parentIds : [
			''f799a4358841dd4c7252338842dc04995f3c5b86'',
			''fc3189a73b6ef736678ac835999c55aa3e416fe3''
		]
	},
	HiExampleCommit {
		#id : ''fc3189a73b6ef736678ac835999c55aa3e416fe3'',
		#comment : ''Merge branch \''dev-1.0\'' into enh/metacello'',
		#parentIds : [
			''15cd66f58070c4bebdf8489ec4fead81fa23876e'',
			''f799a4358841dd4c7252338842dc04995f3c5b86''
		]
	},
	HiExampleCommit {
		#id : ''15cd66f58070c4bebdf8489ec4fead81fa23876e'',
		#comment : ''Use backwards compatible file format (i.e., filetree) in case file format not found'',
		#parentIds : [
			''a24009cc5b6eabc0b0c273bd47f0a3132f2841bd''
		]
	},
	HiExampleCommit {
		#id : ''a24009cc5b6eabc0b0c273bd47f0a3132f2841bd'',
		#comment : ''Add new package to baseline'',
		#parentIds : [
			''786d6f233ca80d8a92813e2e46bee256384faad9''
		]
	},
	HiExampleCommit {
		#id : ''f799a4358841dd4c7252338842dc04995f3c5b86'',
		#comment : ''Merge pull request #891 from tesonep/feature/progressBars\n\nAdding progress bar to long Git operations'',
		#parentIds : [
			''23ef98dab92aa460bc3d25d7215896bfe6d27085'',
			''e1d1f3d661a98621ff90f5c0cf6942421df27c7d''
		]
	},
	HiExampleCommit {
		#id : ''786d6f233ca80d8a92813e2e46bee256384faad9'',
		#comment : ''fixed crs\n'',
		#parentIds : [
			''553e2a0acbff01b5fc749c33571f78961abd4f76''
		]
	},
	HiExampleCommit {
		#id : ''553e2a0acbff01b5fc749c33571f78961abd4f76'',
		#comment : ''Create a project with a source directory'',
		#parentIds : [
			''a0b90974f7595b36efbe778b1ce40c0b906e2897''
		]
	},
	HiExampleCommit {
		#id : ''a0b90974f7595b36efbe778b1ce40c0b906e2897'',
		#comment : ''Split jobs in travis\n'',
		#parentIds : [
			''785bcde65b0c99da1cdd69e8fc9e30a3630f81d3''
		]
	},
	HiExampleCommit {
		#id : ''785bcde65b0c99da1cdd69e8fc9e30a3630f81d3'',
		#comment : ''Split metacello tests in a separate package'',
		#parentIds : [
			''375370548cf5a535456a8fe343c1be0a332ab629''
		]
	},
	HiExampleCommit {
		#id : ''e1d1f3d661a98621ff90f5c0cf6942421df27c7d'',
		#comment : ''Merge branch \''dev-1.0\'' into feature/progressBars'',
		#parentIds : [
			''d39f32b01eb31ff670bda508a64ec88b0a95e47f'',
			''23ef98dab92aa460bc3d25d7215896bfe6d27085''
		]
	},
	HiExampleCommit {
		#id : ''375370548cf5a535456a8fe343c1be0a332ab629'',
		#comment : ''add basicProject'',
		#parentIds : [
			''eb3b0a412597ce11bcc458333c1d599e55c85419''
		]
	},
	HiExampleCommit {
		#id : ''eb3b0a412597ce11bcc458333c1d599e55c85419'',
		#comment : ''Add tonel cases tests'',
		#parentIds : [
			''32d04b4c770889c981ad34880c4bf559584d2302''
		]
	},
	HiExampleCommit {
		#id : ''32d04b4c770889c981ad34880c4bf559584d2302'',
		#comment : ''More metacello tests'',
		#parentIds : [
			''835108a981029fa9a07412075839bed5eeb0cfc2''
		]
	},
	HiExampleCommit {
		#id : ''835108a981029fa9a07412075839bed5eeb0cfc2'',
		#comment : ''return project'',
		#parentIds : [
			''6fb0f9be4ef92f779c9f4a00881cdc4d32ffd969''
		]
	},
	HiExampleCommit {
		#id : ''23ef98dab92aa460bc3d25d7215896bfe6d27085'',
		#comment : ''Merge pull request #890 from guillep/issue/880\n\nFix #880 and #889'',
		#parentIds : [
			''6fb0f9be4ef92f779c9f4a00881cdc4d32ffd969'',
			''95a7733f4d59dee0387c1c2b1d071918827d4d03''
		]
	},
	HiExampleCommit {
		#id : ''6fb0f9be4ef92f779c9f4a00881cdc4d32ffd969'',
		#comment : ''Merge pull request #896 from tesonep/issue/873\n\nissue/873'',
		#parentIds : [
			''3e39cad8225f39627a45635caca2a3f7f28dd1e5'',
			''d9c96b5412f29d346eeedf4c952a6635606d72c1''
		]
	},
	HiExampleCommit {
		#id : ''95a7733f4d59dee0387c1c2b1d071918827d4d03'',
		#comment : ''Use Path API compatible with Pharo6 and Pharo7\n\n(RelativePath withAll: (RelativePath canonicalizeElements: ($/ split: pathString)))'',
		#parentIds : [
			''20b4afcde770cfe8985de371cfcace4eec266e74''
		]
	},
	HiExampleCommit {
		#id : ''20b4afcde770cfe8985de371cfcace4eec266e74'',
		#comment : ''Merge 3e39cad8225f39627a45635caca2a3f7f28dd1e5'',
		#parentIds : [
			''a6339436556183da71ff1a565bdb68e7b0be5ce3'',
			''3e39cad8225f39627a45635caca2a3f7f28dd1e5''
		]
	},
	HiExampleCommit {
		#id : ''3e39cad8225f39627a45635caca2a3f7f28dd1e5'',
		#comment : ''remove halt'',
		#parentIds : [
			''6286e758f1419dc28a667fa952bbaa3b17029941''
		]
	},
	HiExampleCommit {
		#id : ''6286e758f1419dc28a667fa952bbaa3b17029941'',
		#comment : ''Fixed bootstrap with project'',
		#parentIds : [
			''8b6bcb05bfe478aec72fc88db0376d64c32bab01''
		]
	},
	HiExampleCommit {
		#id : ''8b6bcb05bfe478aec72fc88db0376d64c32bab01'',
		#comment : ''Merge pull request #898 from guillep/issue/897\n\nFix #897'',
		#parentIds : [
			''010bc3bf427bfd54c8b9c4f23d74abd29756959d'',
			''ade1925781756b5926aa56a234a84473d61ebdd1''
		]
	},
	HiExampleCommit {
		#id : ''ade1925781756b5926aa56a234a84473d61ebdd1'',
		#comment : ''Fix #897'',
		#parentIds : [
			''caade1892bfeb00413ff49b32baa94bd81d04997''
		]
	},
	HiExampleCommit {
		#id : ''d9c96b5412f29d346eeedf4c952a6635606d72c1'',
		#comment : ''Adding dialog to ask the ssh keys when there are no SSH credential configured.'',
		#parentIds : [
			''8f459d36a12dfe47b014c08e4f9cdfe3f9c9ffb9''
		]
	},
	HiExampleCommit {
		#id : ''010bc3bf427bfd54c8b9c4f23d74abd29756959d'',
		#comment : ''Merge pull request #895 from gcotelli/githubapidelete\n\nFixes case when DELETE is returning a 204 No Content, for example usiâ\u0080¦'',
		#parentIds : [
			''7d4d70218e0f4b6fb8901c44f101dcc21343902e'',
			''3770cf9355c1ded495d6aa48a3a46ef7f7945194''
		]
	},
	HiExampleCommit {
		#id : ''a6339436556183da71ff1a565bdb68e7b0be5ce3'',
		#comment : ''Merge 7d4d70218e0f4b6fb8901c44f101dcc21343902e'',
		#parentIds : [
			''0b0a0f9b29c905a454bc02151d96beb3ff985809'',
			''7d4d70218e0f4b6fb8901c44f101dcc21343902e''
		]
	},
	HiExampleCommit {
		#id : ''0b0a0f9b29c905a454bc02151d96beb3ff985809'',
		#comment : ''Better management of unborn projects for memory backend tests'',
		#parentIds : [
			''c7eac5c4fd0d5152558084dce4e190841dd70b52''
		]
	},
	HiExampleCommit {
		#id : ''3770cf9355c1ded495d6aa48a3a46ef7f7945194'',
		#comment : ''Fixes case when DELETE is returning a 204 No Content, for example using the API to delete labels in a project. '',
		#parentIds : [
			''83285da7c99bff417054c1fa7d8ab7f6f64144b2''
		]
	},
	HiExampleCommit {
		#id : ''7d4d70218e0f4b6fb8901c44f101dcc21343902e'',
		#comment : ''Merge pull request #893 from guillep/issue/839\n\nFix #839'',
		#parentIds : [
			''83285da7c99bff417054c1fa7d8ab7f6f64144b2'',
			''5ec5ccda998efe106adb92e89aca815ba10678d6''
		]
	},
	HiExampleCommit {
		#id : ''5ec5ccda998efe106adb92e89aca815ba10678d6'',
		#comment : ''Using a relative path'',
		#parentIds : [
			''0af37b1cb342328729759bea6066eba375b16125''
		]
	},
	HiExampleCommit {
		#id : ''0af37b1cb342328729759bea6066eba375b16125'',
		#comment : ''Fix #839'',
		#parentIds : [
			''caade1892bfeb00413ff49b32baa94bd81d04997''
		]
	},
	HiExampleCommit {
		#id : ''c7eac5c4fd0d5152558084dce4e190841dd70b52'',
		#comment : ''Avoid duplicating \''.\'' string'',
		#parentIds : [
			''c08f13dee889577d8952321b82941b0e0f6beccd''
		]
	},
	HiExampleCommit {
		#id : ''c08f13dee889577d8952321b82941b0e0f6beccd'',
		#comment : ''Add tests for #880'',
		#parentIds : [
			''caade1892bfeb00413ff49b32baa94bd81d04997''
		]
	},
	HiExampleCommit {
		#id : ''d39f32b01eb31ff670bda508a64ec88b0a95e47f'',
		#comment : ''Fixing problem with the lazy initialization.'',
		#parentIds : [
			''4b88735d943f5aa1f5cf72607ff165c38a92796f''
		]
	},
	HiExampleCommit {
		#id : ''4b88735d943f5aa1f5cf72607ff165c38a92796f'',
		#comment : ''Updating the version of libgit2\n'',
		#parentIds : [
			''64c60843124b0cf0513b6e1f351a37ec197a977c''
		]
	},
	HiExampleCommit {
		#id : ''caade1892bfeb00413ff49b32baa94bd81d04997'',
		#comment : ''Fix #880'',
		#parentIds : [
			''83285da7c99bff417054c1fa7d8ab7f6f64144b2''
		]
	},
	HiExampleCommit {
		#id : ''83285da7c99bff417054c1fa7d8ab7f6f64144b2'',
		#comment : ''Merge pull request #887 from tesonep/issue/884\n\nImproving the edit project dialog.'',
		#parentIds : [
			''8f459d36a12dfe47b014c08e4f9cdfe3f9c9ffb9'',
			''c645ae43d0687559b7da05d28aa3c97c1b30b802''
		]
	},
	HiExampleCommit {
		#id : ''64c60843124b0cf0513b6e1f351a37ec197a977c'',
		#comment : ''Push Tranfer progress'',
		#parentIds : [
			''5a45eaf9c0e853c008de2600d2e8cdfa5663f349''
		]
	},
	HiExampleCommit {
		#id : ''5a45eaf9c0e853c008de2600d2e8cdfa5663f349'',
		#comment : ''Fix the could not locate repository action.'',
		#parentIds : [
			''d691dae34af49ec865fc86d4822d735cc0050c46''
		]
	},
	HiExampleCommit {
		#id : ''d691dae34af49ec865fc86d4822d735cc0050c46'',
		#comment : ''Adding progress bars to Fetch and Checkout.'',
		#parentIds : [
			''8f459d36a12dfe47b014c08e4f9cdfe3f9c9ffb9''
		]
	},
	HiExampleCommit {
		#id : ''c645ae43d0687559b7da05d28aa3c97c1b30b802'',
		#comment : ''Improving the edit project dialog.'',
		#parentIds : [
			''a12bcb92060b2d6ba75b2d5b6d75f48c664b8ea0''
		]
	},
	HiExampleCommit {
		#id : ''8f459d36a12dfe47b014c08e4f9cdfe3f9c9ffb9'',
		#comment : ''Merge pull request #882 from tesonep/issue/870\n\nAdding Metacello Integration tests and fixing the detected problems'',
		#parentIds : [
			''a12bcb92060b2d6ba75b2d5b6d75f48c664b8ea0'',
			''0698567486d8bd5608bd01905f4d0ad7d5556355''
		]
	},
	HiExampleCommit {
		#id : ''0698567486d8bd5608bd01905f4d0ad7d5556355'',
		#comment : ''Modifying tests to make them use the configurable flag'',
		#parentIds : [
			''ac5ebc04bfe7b4c0ce9e2137b1c2d2bcfbd36a04''
		]
	},
	HiExampleCommit {
		#id : ''ac5ebc04bfe7b4c0ce9e2137b1c2d2bcfbd36a04'',
		#comment : ''Fixing the tests'',
		#parentIds : [
			''0128353d9a796e3fafe3d1dfcf0ae2bc024f6e4b''
		]
	},
	HiExampleCommit {
		#id : ''0128353d9a796e3fafe3d1dfcf0ae2bc024f6e4b'',
		#comment : ''This test should be configurable'',
		#parentIds : [
			''a11609a735ffacf15e9db3306905aaf1bcf8ae30''
		]
	},
	HiExampleCommit {
		#id : ''a11609a735ffacf15e9db3306905aaf1bcf8ae30'',
		#comment : ''Fixing the equals of projects'',
		#parentIds : [
			''a663f2b985d4139e520ca8defa8b1aaf6d20f6ef''
		]
	},
	HiExampleCommit {
		#id : ''a663f2b985d4139e520ca8defa8b1aaf6d20f6ef'',
		#comment : ''Fixing the dirtiness in the memory project'',
		#parentIds : [
			''2b0da0c449bbccd782fbb30a7d516cda40ec2d75''
		]
	},
	HiExampleCommit {
		#id : ''2b0da0c449bbccd782fbb30a7d516cda40ec2d75'',
		#comment : ''Fixing the fileSystem of IceWorkingCopy'',
		#parentIds : [
			''d20ae96c52def58ab3a645b03b56a158f1d320f6''
		]
	},
	HiExampleCommit {
		#id : ''d20ae96c52def58ab3a645b03b56a158f1d320f6'',
		#comment : ''Handling dirtiness when the project is dirty'',
		#parentIds : [
			''f0c31f2f303e9c5357573cc7ffd33b01ef3e7bd5''
		]
	},
	HiExampleCommit {
		#id : ''f0c31f2f303e9c5357573cc7ffd33b01ef3e7bd5'',
		#comment : ''All the repo have project now.'',
		#parentIds : [
			''23aa8db855cbdfe62599bcee0d744d1c72ddf9ed''
		]
	},
	HiExampleCommit {
		#id : ''23aa8db855cbdfe62599bcee0d744d1c72ddf9ed'',
		#comment : ''Fixing when the source directory is nil.'',
		#parentIds : [
			''73681378546d859d6e8f09b35a2a16560cfed4a1''
		]
	},
	HiExampleCommit {
		#id : ''73681378546d859d6e8f09b35a2a16560cfed4a1'',
		#comment : ''Fix issue #874\nEnsure that the repositories created from metacello have a project .\nAlso adds test.'',
		#parentIds : [
			''e0edc95d12ff942332d65b0077c2ef79b743d308''
		]
	},
	HiExampleCommit {
		#id : ''e0edc95d12ff942332d65b0077c2ef79b743d308'',
		#comment : ''Fixing the test to run correctly'',
		#parentIds : [
			''d0dbdb774e1ab2cb43f3401848134ffa9c92da54''
		]
	},
	HiExampleCommit {
		#id : ''d0dbdb774e1ab2cb43f3401848134ffa9c92da54'',
		#comment : ''This test should not be skipped'',
		#parentIds : [
			''c427b321030bd03e890be24f3cdf59e94fc61708''
		]
	},
	HiExampleCommit {
		#id : ''c427b321030bd03e890be24f3cdf59e94fc61708'',
		#comment : ''Fixing test because SmalltalkCI does not support parametrized tests yet.'',
		#parentIds : [
			''02634e7999dee94bf43e5a649265192d3310739e''
		]
	},
	HiExampleCommit {
		#id : ''02634e7999dee94bf43e5a649265192d3310739e'',
		#comment : ''Adding more Metacello Integration tests'',
		#parentIds : [
			''c9343f57dad5fad17ba5a9ea16eecae79df7ed33''
		]
	},
	HiExampleCommit {
		#id : ''c9343f57dad5fad17ba5a9ea16eecae79df7ed33'',
		#comment : ''Fixing tests'',
		#parentIds : [
			''eb4b21bc1e0ed0c2b7ae77b22fdbd58de1ef09d5''
		]
	},
	HiExampleCommit {
		#id : ''eb4b21bc1e0ed0c2b7ae77b22fdbd58de1ef09d5'',
		#comment : ''Fixing the test to run in the CI'',
		#parentIds : [
			''ab4c9b464317d3ffc93d55318b46067742511d5e''
		]
	},
	HiExampleCommit {
		#id : ''ab4c9b464317d3ffc93d55318b46067742511d5e'',
		#comment : ''Adding tests to support combinations with project / src / filetree '',
		#parentIds : [
			''a12bcb92060b2d6ba75b2d5b6d75f48c664b8ea0''
		]
	},
	HiExampleCommit {
		#id : ''a12bcb92060b2d6ba75b2d5b6d75f48c664b8ea0'',
		#comment : ''Merge pull request #867 from guillep/feature/projects\n\nIntroduce first version of Projects'',
		#parentIds : [
			''7c1ae1386846926a66e287dfe93926d3cf9d8643'',
			''2d26ec68ee73822821975e32401c6b4ec4cd3295''
		]
	},
	HiExampleCommit {
		#id : ''2d26ec68ee73822821975e32401c6b4ec4cd3295'',
		#comment : ''Pre-Unload Migration Plugin in CI'',
		#parentIds : [
			''1dee1eafb80f16c45676838622fef4ee218ecc08''
		]
	},
	HiExampleCommit {
		#id : ''1dee1eafb80f16c45676838622fef4ee218ecc08'',
		#comment : ''Re-organizing tests'',
		#parentIds : [
			''d87a556ece02be6aef3816399f62b9af4b226136''
		]
	},
	HiExampleCommit {
		#id : ''7c1ae1386846926a66e287dfe93926d3cf9d8643'',
		#comment : ''Merge pull request #846 from dionisiydk/useCommander063\n\nuseCommander063'',
		#parentIds : [
			''726254360c987b9b3472a6098b5869970e819cc9'',
			''ae64bc4c99dcd4b6e34044c837cc1a681598f466''
		]
	},
	HiExampleCommit {
		#id : ''d87a556ece02be6aef3816399f62b9af4b226136'',
		#comment : ''Rest of manual merge'',
		#parentIds : [
			''b2998f6cf03ea241a88ad189010bcc1ac5833d48''
		]
	},
	HiExampleCommit {
		#id : ''b2998f6cf03ea241a88ad189010bcc1ac5833d48'',
		#comment : ''Merge 726254360c987b9b3472a6098b5869970e819cc9'',
		#parentIds : [
			''3a3a1277530ec92e6b78f232c188bdfd32bf2359'',
			''726254360c987b9b3472a6098b5869970e819cc9''
		]
	},
	HiExampleCommit {
		#id : ''726254360c987b9b3472a6098b5869970e819cc9'',
		#comment : ''Fix #864\nHandle missing code subdirectory'',
		#parentIds : [
			''4f9e2b5ae380954c5d48fbc6f747f6573fff1065''
		]
	},
	HiExampleCommit {
		#id : ''4f9e2b5ae380954c5d48fbc6f747f6573fff1065'',
		#comment : ''Merge pull request #861 from estebanlm/dev-1.0\n\nupdate tonel to v1.0.9'',
		#parentIds : [
			''5684a9eac01c155a16be0428b6690fc71869d087'',
			''53e71948c2ffade80cf0ce894ff4bc8fa1e2b891''
		]
	},
	HiExampleCommit {
		#id : ''5684a9eac01c155a16be0428b6690fc71869d087'',
		#comment : ''Merge e93cd19b7e9433a04a974b7ce86cca504ad40971'',
		#parentIds : [
			''5719ed46cb67f257b7dc558e21c58cdc951bfb91'',
			''e93cd19b7e9433a04a974b7ce86cca504ad40971''
		]
	},
	HiExampleCommit {
		#id : ''5719ed46cb67f257b7dc558e21c58cdc951bfb91'',
		#comment : ''Fix #836\nRemove unused class variable'',
		#parentIds : [
			''b174d22c625b51832025b6af1adc5896f239a060''
		]
	},
	HiExampleCommit {
		#id : ''e93cd19b7e9433a04a974b7ce86cca504ad40971'',
		#comment : ''Merge pull request #863 from pharo-vcs/issue/862\n\nFix #862'',
		#parentIds : [
			''b174d22c625b51832025b6af1adc5896f239a060'',
			''b90413d2f6cf561a967f7d4e223cc10a90fbff2b''
		]
	},
	HiExampleCommit {
		#id : ''b90413d2f6cf561a967f7d4e223cc10a90fbff2b'',
		#comment : ''Fix unload script'',
		#parentIds : [
			''1728953e579e107f874d1b1e6081aafc9c0fce62''
		]
	},
	HiExampleCommit {
		#id : ''1728953e579e107f874d1b1e6081aafc9c0fce62'',
		#comment : ''Fix #862\n\nUnregister all iceberg repository adapters since we are going to unload all code related to it.\r\nOtherwise obsolete instances will stay"'',
		#parentIds : [
			''b174d22c625b51832025b6af1adc5896f239a060''
		]
	},
	HiExampleCommit {
		#id : ''b174d22c625b51832025b6af1adc5896f239a060'',
		#comment : ''Fix #852\nUse newIceReadOnlyText in error dialog to allow copy paste.'',
		#parentIds : [
			''b672d017cfe2ced2c5c2f5b8ac133f61a71eaf24''
		]
	},
	HiExampleCommit {
		#id : ''b672d017cfe2ced2c5c2f5b8ac133f61a71eaf24'',
		#comment : ''Fix #858\n- Moved event handling to subclass (IceTipLabelMorph)'',
		#parentIds : [
			''6c363f7dfa718b24726b4704ae88a5708e57ea58''
		]
	},
	HiExampleCommit {
		#id : ''6c363f7dfa718b24726b4704ae88a5708e57ea58'',
		#comment : ''Fix #850\n- Make text in warning color (set by theme) if head is tagged'',
		#parentIds : [
			''a2582894b563872fb61144f2ed4062b73501064f''
		]
	},
	HiExampleCommit {
		#id : ''a2582894b563872fb61144f2ed4062b73501064f'',
		#comment : ''Fix #853\nReplace "Github/Gitlab/Bitbucket username" for "Owner name"'',
		#parentIds : [
			''e888c6fba3d37a914ea7114c64eaf36c41e19e55''
		]
	},
	HiExampleCommit {
		#id : ''53e71948c2ffade80cf0ce894ff4bc8fa1e2b891'',
		#comment : ''Merge e888c6fba3d37a914ea7114c64eaf36c41e19e55'',
		#parentIds : [
			''c4df21e5a0b9595a0409d8c2d1f28b3e848caead'',
			''e888c6fba3d37a914ea7114c64eaf36c41e19e55''
		]
	},
	HiExampleCommit {
		#id : ''c4df21e5a0b9595a0409d8c2d1f28b3e848caead'',
		#comment : ''update tonel to v1.0.9'',
		#parentIds : [
			''5a9aaebdc70667548ab4c84ffdb6da309c31df6a''
		]
	},
	HiExampleCommit {
		#id : ''e888c6fba3d37a914ea7114c64eaf36c41e19e55'',
		#comment : ''Merge pull request #860 from tesonep/dev-1.0\n\nCredentialStore API'',
		#parentIds : [
			''63f7d4b694c7bbf6ccc98c4103fcd527899cc3b0'',
			''51cbb52a4dd2e4f223b92f12d03d69e9b748c5c4''
		]
	},
	HiExampleCommit {
		#id : ''51cbb52a4dd2e4f223b92f12d03d69e9b748c5c4'',
		#comment : ''Better comment'',
		#parentIds : [
			''f75febab6ed1d19b882e15379e6f2f3c65a7e3d5''
		]
	},
	HiExampleCommit {
		#id : ''5a9aaebdc70667548ab4c84ffdb6da309c31df6a'',
		#comment : ''update tonel to v1.0.8 '',
		#parentIds : [
			''63f7d4b694c7bbf6ccc98c4103fcd527899cc3b0''
		]
	},
	HiExampleCommit {
		#id : ''f75febab6ed1d19b882e15379e6f2f3c65a7e3d5'',
		#comment : ''Adding deprecation of old CredentialProvider class API, and adding tests.'',
		#parentIds : [
			''63f7d4b694c7bbf6ccc98c4103fcd527899cc3b0''
		]
	},
	HiExampleCommit {
		#id : ''3a3a1277530ec92e6b78f232c188bdfd32bf2359'',
		#comment : ''Make tests run green'',
		#parentIds : [
			''8ba46c807d59c29da455934526c233ef7a7c8cf1''
		]
	},
	HiExampleCommit {
		#id : ''8ba46c807d59c29da455934526c233ef7a7c8cf1'',
		#comment : ''Better IceTipEditProjectDialog selection'',
		#parentIds : [
			''41c449902db8a106326863e568bc51a7e222afb2''
		]
	},
	HiExampleCommit {
		#id : ''41c449902db8a106326863e568bc51a7e222afb2'',
		#comment : ''Fix migration tests'',
		#parentIds : [
			''67efa38b712896279ec6d7f9941105e534a57491''
		]
	},
	HiExampleCommit {
		#id : ''67efa38b712896279ec6d7f9941105e534a57491'',
		#comment : ''Edit Repository uses new edit repository dialog'',
		#parentIds : [
			''4b5eb67fae5f2699d6b4480ee182de3a4cb4a194''
		]
	},
	HiExampleCommit {
		#id : ''4b5eb67fae5f2699d6b4480ee182de3a4cb4a194'',
		#comment : ''Cleanup in IceProject'',
		#parentIds : [
			''d5808636de1e3b04e3db331a081b899c30bb0488''
		]
	},
	HiExampleCommit {
		#id : ''d5808636de1e3b04e3db331a081b899c30bb0488'',
		#comment : ''Recognize project and properties files in diff tree'',
		#parentIds : [
			''d345869001f2278e2453de0a21c7b06c2554371d''
		]
	},
	HiExampleCommit {
		#id : ''d345869001f2278e2453de0a21c7b06c2554371d'',
		#comment : ''Fixed merge test\n - a new project should be initialized with a format'',
		#parentIds : [
			''d6e560c57ded260bb0b271e0838ee02484487f0a''
		]
	},
	HiExampleCommit {
		#id : ''d6e560c57ded260bb0b271e0838ee02484487f0a'',
		#comment : ''Fix migration tests'',
		#parentIds : [
			''c487b8fcef4be8ae5adb62f1f7d99e78a7c0107a''
		]
	},
	HiExampleCommit {
		#id : ''63f7d4b694c7bbf6ccc98c4103fcd527899cc3b0'',
		#comment : ''Fixing problem when the iceberg directory in pharo-local does not exists.'',
		#parentIds : [
			''07c8fdb10fdba7d1885c5339d1f16bddad5433f6''
		]
	},
	HiExampleCommit {
		#id : ''c487b8fcef4be8ae5adb62f1f7d99e78a7c0107a'',
		#comment : ''Make project properties reside in memory'',
		#parentIds : [
			''7d9da13565b8d6d7283c3f23ee49c578491c60b5''
		]
	},
	HiExampleCommit {
		#id : ''7d9da13565b8d6d7283c3f23ee49c578491c60b5'',
		#comment : ''Fix all tests but metacello integration'',
		#parentIds : [
			''9a2917bf2d80b89a6f98b550958f3735b2a64223''
		]
	},
	HiExampleCommit {
		#id : ''9a2917bf2d80b89a6f98b550958f3735b2a64223'',
		#comment : ''Update properties file'',
		#parentIds : [
			''7454ca90b0500360c3b30970f0913d6948d4acc2''
		]
	},
	HiExampleCommit {
		#id : ''7454ca90b0500360c3b30970f0913d6948d4acc2'',
		#comment : ''Fix all tests except metacello integration'',
		#parentIds : [
			''67ce629a36049afb344e9704310c3ce5d1a1816c''
		]
	},
	HiExampleCommit {
		#id : ''07c8fdb10fdba7d1885c5339d1f16bddad5433f6'',
		#comment : ''Merge pull request #855 from jecisc/addMissingMethods\n\naddMissingMethods'',
		#parentIds : [
			''02cd72913ebec2be6304981932c602c8e125ee3f'',
			''3a25ed23f29072a4c8b521b0899eb0f3dd9623ef''
		]
	},
	HiExampleCommit {
		#id : ''3a25ed23f29072a4c8b521b0899eb0f3dd9623ef'',
		#comment : ''Add missing methods. Fixes https://github.com/pharo-vcs/iceberg/issues/854'',
		#parentIds : [
			''02cd72913ebec2be6304981932c602c8e125ee3f''
		]
	},
	HiExampleCommit {
		#id : ''67ce629a36049afb344e9704310c3ce5d1a1816c'',
		#comment : ''First version of project edition dialog'',
		#parentIds : [
			''50a1bc303ea6ae5c9bb04196999b9b368a1ec938''
		]
	},
	HiExampleCommit {
		#id : ''50a1bc303ea6ae5c9bb04196999b9b368a1ec938'',
		#comment : ''Managing unborn projects in the UI'',
		#parentIds : [
			''929bc38243b723e82ba3cfeb2920c52f587218ea''
		]
	},
	HiExampleCommit {
		#id : ''929bc38243b723e82ba3cfeb2920c52f587218ea'',
		#comment : ''Add project file'',
		#parentIds : [
			''578095848ee789db56e8c2a3ed07cfd5d21539b6''
		]
	},
	HiExampleCommit {
		#id : ''578095848ee789db56e8c2a3ed07cfd5d21539b6'',
		#comment : ''Add Project'',
		#parentIds : [
			''02cd72913ebec2be6304981932c602c8e125ee3f''
		]
	},
	HiExampleCommit {
		#id : ''02cd72913ebec2be6304981932c602c8e125ee3f'',
		#comment : ''Do not check for registry conflicts if repository location is nil'',
		#parentIds : [
			''669eecced2f03f6187d7a44ab371dd2c970062bb''
		]
	},
	HiExampleCommit {
		#id : ''ae64bc4c99dcd4b6e34044c837cc1a681598f466'',
		#comment : ''new Commander with deprecated renamed classes'',
		#parentIds : [
			''fb1e1e8a32a91f06584b395a26f4ce52b7077824''
		]
	},
	HiExampleCommit {
		#id : ''669eecced2f03f6187d7a44ab371dd2c970062bb'',
		#comment : ''Merge fb1e1e8a32a91f06584b395a26f4ce52b7077824'',
		#parentIds : [
			''7474e2513df95285c7158ca125a57942d660ad5e'',
			''fb1e1e8a32a91f06584b395a26f4ce52b7077824''
		]
	},
	HiExampleCommit {
		#id : ''fb1e1e8a32a91f06584b395a26f4ce52b7077824'',
		#comment : ''Merge pull request #842 from tesonep/issue/763\n\nIssue/763 - Adding Credentials Store'',
		#parentIds : [
			''c0d745d513bca125b1701824f9a117fbb75874af'',
			''edf7ffbfb26fab10b555f1eab29c45fd95876c09''
		]
	},
	HiExampleCommit {
		#id : ''c0d745d513bca125b1701824f9a117fbb75874af'',
		#comment : ''Merge pull request #845 from jecisc/excludeCurrentPackagesFromAddPackageDialog\n\nImprove add packages dialog'',
		#parentIds : [
			''52f809b42c0bfb29967c58e8826c8ae06fd581ad'',
			''961bda5b0bdd00fac670cd1bc5c8e13e611ecfd9''
		]
	},
	HiExampleCommit {
		#id : ''961bda5b0bdd00fac670cd1bc5c8e13e611ecfd9'',
		#comment : ''Add package dialog: Rename "Ok" button to "Add"'',
		#parentIds : [
			''76a81477fbe0032e16ecef9b996c986306b9dfde''
		]
	},
	HiExampleCommit {
		#id : ''76a81477fbe0032e16ecef9b996c986306b9dfde'',
		#comment : ''Disable "ok" button from "Add packages" dialog if there is no selected item.'',
		#parentIds : [
			''e3c3f4fd66662d7db9e724d4fd6618399b24eeb2''
		]
	},
	HiExampleCommit {
		#id : ''e3c3f4fd66662d7db9e724d4fd6618399b24eeb2'',
		#comment : ''In "Add Packages" dialog, reject packages already presents.'',
		#parentIds : [
			''52f809b42c0bfb29967c58e8826c8ae06fd581ad''
		]
	},
	HiExampleCommit {
		#id : ''52f809b42c0bfb29967c58e8826c8ae06fd581ad'',
		#comment : ''update to tonel 1.0.7'',
		#parentIds : [
			''ae3d0fbf083df2a9bb58f6ad63b2cdd63c11d7fd''
		]
	},
	HiExampleCommit {
		#id : ''edf7ffbfb26fab10b555f1eab29c45fd95876c09'',
		#comment : ''The git clone validates the URL provided.'',
		#parentIds : [
			''fb968780d9634fb87870d04e8deb71d6c12e0316''
		]
	},
	HiExampleCommit {
		#id : ''fb968780d9634fb87870d04e8deb71d6c12e0316'',
		#comment : ''Merge branch \''dev-1.0\'' into issue/763\n'',
		#parentIds : [
			''f0b142813c03de8807aa1fb966111f1d61fb0807'',
			''ae3d0fbf083df2a9bb58f6ad63b2cdd63c11d7fd''
		]
	},
	HiExampleCommit {
		#id : ''f0b142813c03de8807aa1fb966111f1d61fb0807'',
		#comment : ''Fixing the fetch'',
		#parentIds : [
			''6b3ac343bfc59f4d7b446b311cc739bb46471bd9''
		]
	},
	HiExampleCommit {
		#id : ''6b3ac343bfc59f4d7b446b311cc739bb46471bd9'',
		#comment : ''Fixing when modified using the settings'',
		#parentIds : [
			''236304f95c25661c89f55062c06f6bae68da9f0e''
		]
	},
	HiExampleCommit {
		#id : ''236304f95c25661c89f55062c06f6bae68da9f0e'',
		#comment : ''Adding comments.'',
		#parentIds : [
			''a269e4f4758b8a49d279bea25db997612b51c5dc''
		]
	},
	HiExampleCommit {
		#id : ''a269e4f4758b8a49d279bea25db997612b51c5dc'',
		#comment : ''Adding support for adding and editing SSH keys'',
		#parentIds : [
			''1a4159b4510ff7fe793b9496040a08144d2e2082''
		]
	},
	HiExampleCommit {
		#id : ''ae3d0fbf083df2a9bb58f6ad63b2cdd63c11d7fd'',
		#comment : ''Merge pull request #841 from pharo-vcs/feature/tags\n\nBasic tag support'',
		#parentIds : [
			''069519657bdd200e86aa21aba8ebf537f79ab9a7'',
			''933f38b9b94f669919e9125a4fee203c85c9a35e''
		]
	},
	HiExampleCommit {
		#id : ''933f38b9b94f669919e9125a4fee203c85c9a35e'',
		#comment : ''Using libgit v1.4.0'',
		#parentIds : [
			''b7554613a4b44a51fab7462880ac08ba4be5f541''
		]
	},
	HiExampleCommit {
		#id : ''b7554613a4b44a51fab7462880ac08ba4be5f541'',
		#comment : ''Better management of text morphs'',
		#parentIds : [
			''5d4f258eb848e6e9f3c3770a0ac488d9d0d2c2be''
		]
	},
	HiExampleCommit {
		#id : ''1a4159b4510ff7fe793b9496040a08144d2e2082'',
		#comment : ''Adding Edit and Add dialogs.'',
		#parentIds : [
			''cf477578ac30cd1a9ef35edb0e0e7a007144ced1''
		]
	},
	HiExampleCommit {
		#id : ''5d4f258eb848e6e9f3c3770a0ac488d9d0d2c2be'',
		#comment : ''Pushing tags'',
		#parentIds : [
			''6332a8458378efe4979fa6a70fc11daead174530''
		]
	},
	HiExampleCommit {
		#id : ''6332a8458378efe4979fa6a70fc11daead174530'',
		#comment : ''Test remove tag and access non existing tags'',
		#parentIds : [
			''dfd8dbfe27aadaa58556c9b0109a030dcf2aeee3''
		]
	},
	HiExampleCommit {
		#id : ''dfd8dbfe27aadaa58556c9b0109a030dcf2aeee3'',
		#comment : ''Implement tag delete'',
		#parentIds : [
			''8414a9b0cd7c7a62532840af2ec630d003dee70f''
		]
	},
	HiExampleCommit {
		#id : ''8414a9b0cd7c7a62532840af2ec630d003dee70f'',
		#comment : ''Rename branchModelsByGroup => repositoryModelsByGroup to make it tag-compatible'',
		#parentIds : [
			''14d784f5aa6f857f09b05720d1604de49ce3efe6''
		]
	},
	HiExampleCommit {
		#id : ''14d784f5aa6f857f09b05720d1604de49ce3efe6'',
		#comment : ''Add tags in sidebar'',
		#parentIds : [
			''35d72d734792ffbc2a5e1b9ab89c19ba7bb41da0''
		]
	},
	HiExampleCommit {
		#id : ''35d72d734792ffbc2a5e1b9ab89c19ba7bb41da0'',
		#comment : ''Fix variable name'',
		#parentIds : [
			''155a379f52b99b3ff25b588b439ee779bbe26460''
		]
	},
	HiExampleCommit {
		#id : ''155a379f52b99b3ff25b588b439ee779bbe26460'',
		#comment : ''Add tags in commit info'',
		#parentIds : [
			''09468af7c9b24093e9a20d7dffb1f2229d1d84ab''
		]
	},
	HiExampleCommit {
		#id : ''09468af7c9b24093e9a20d7dffb1f2229d1d84ab'',
		#comment : ''Fix create tag from history browser'',
		#parentIds : [
			''fdf54c2cb79ff6f7ec7a7b4c681034a87f56105d''
		]
	},
	HiExampleCommit {
		#id : ''fdf54c2cb79ff6f7ec7a7b4c681034a87f56105d'',
		#comment : ''Show tags in commit list'',
		#parentIds : [
			''38bafac45d7853234b8fe652a52b4eedade67b0e''
		]
	},
	HiExampleCommit {
		#id : ''38bafac45d7853234b8fe652a52b4eedade67b0e'',
		#comment : ''Add first tag creation UI'',
		#parentIds : [
			''92a05d4e3a9faf24c90a77a874bdbef5b84c328b''
		]
	},
	HiExampleCommit {
		#id : ''92a05d4e3a9faf24c90a77a874bdbef5b84c328b'',
		#comment : ''Show tags in Iceberg UI'',
		#parentIds : [
			''b575927009912d9526feaedd36fe2d04d16f30a1''
		]
	},
	HiExampleCommit {
		#id : ''b575927009912d9526feaedd36fe2d04d16f30a1'',
		#comment : ''Clean branch access'',
		#parentIds : [
			''666e0a0011fa244a8f5b62b7663f52950a2dc7d3''
		]
	},
	HiExampleCommit {
		#id : ''666e0a0011fa244a8f5b62b7663f52950a2dc7d3'',
		#comment : ''First support for tags'',
		#parentIds : [
			''069519657bdd200e86aa21aba8ebf537f79ab9a7''
		]
	},
	HiExampleCommit {
		#id : ''cf477578ac30cd1a9ef35edb0e0e7a007144ced1'',
		#comment : ''Merge branch \''dev-1.0\'' into issue/763\n'',
		#parentIds : [
			''6de9e19ffe18d2a20c3e17d0b6a8a4ecd1e65f6b'',
			''069519657bdd200e86aa21aba8ebf537f79ab9a7''
		]
	},
	HiExampleCommit {
		#id : ''6de9e19ffe18d2a20c3e17d0b6a8a4ecd1e65f6b'',
		#comment : ''Storing the credentials in a local directory file.'',
		#parentIds : [
			''cd7d7133447de7b06af1fbbbe5a508fadbacfb28''
		]
	},
	HiExampleCommit {
		#id : ''069519657bdd200e86aa21aba8ebf537f79ab9a7'',
		#comment : ''Fix #838: better management of clone exceptions'',
		#parentIds : [
			''4e92ba6f69ae245bcd1d60755d688b2ca06cb497''
		]
	},
	HiExampleCommit {
		#id : ''4e92ba6f69ae245bcd1d60755d688b2ca06cb497'',
		#comment : ''Merge pull request #835 from Ducasse/dev-1.0\n\nCompare file definitions by their binary uninterpreted content'',
		#parentIds : [
			''c9c3ac109b3c60177f67a3d507882f1296389323'',
			''35f3ebadfad366aaf03a67956b3eabee0f47dbae''
		]
	},
	HiExampleCommit {
		#id : ''35f3ebadfad366aaf03a67956b3eabee0f47dbae'',
		#comment : ''Compare file definitions by their binary uninterpreted content'',
		#parentIds : [
			''c9c3ac109b3c60177f67a3d507882f1296389323''
		]
	},
	HiExampleCommit {
		#id : ''cd7d7133447de7b06af1fbbbe5a508fadbacfb28'',
		#comment : ''- Adding UI tests\n- Using the new credential Store.\n- Handling adding to the settings.'',
		#parentIds : [
			''82fe67fb22c791379c9e41d55b61645e193849af''
		]
	},
	HiExampleCommit {
		#id : ''c9c3ac109b3c60177f67a3d507882f1296389323'',
		#comment : ''Merge pull request #834 from estebanlm/issue-833\n\nissue-833'',
		#parentIds : [
			''7e744b4f11784cf600c8ef4ba8f4070013600040'',
			''d5c21667f748614f19ccec78bb78d740ebcefc1c''
		]
	},
	HiExampleCommit {
		#id : ''d5c21667f748614f19ccec78bb78d740ebcefc1c'',
		#comment : ''replace usage of #asIcon with #iconNamed:\n\nFixes #833'',
		#parentIds : [
			''7e744b4f11784cf600c8ef4ba8f4070013600040''
		]
	},
	HiExampleCommit {
		#id : ''82fe67fb22c791379c9e41d55b61645e193849af'',
		#comment : ''Integrating the ask key.'',
		#parentIds : [
			''3e39465cb7454d1c3064b389cf2749dbe439853f''
		]
	},
	HiExampleCommit {
		#id : ''3e39465cb7454d1c3064b389cf2749dbe439853f'',
		#comment : ''Creating a credential store to have different credentials depending of the host you are using.'',
		#parentIds : [
			''7e744b4f11784cf600c8ef4ba8f4070013600040''
		]
	},
	HiExampleCommit {
		#id : ''7e744b4f11784cf600c8ef4ba8f4070013600040'',
		#comment : ''Fix #832\nChange menu entry to fix new Pharo layout'',
		#parentIds : [
			''05923f03983bf7904f6b7c8a0ef56e826267d010''
		]
	},
	HiExampleCommit {
		#id : ''05923f03983bf7904f6b7c8a0ef56e826267d010'',
		#comment : ''Make postload more robust for smalltalkCI'',
		#parentIds : [
			''f6f253136e6621969cadc4099c1b599d1468b91d''
		]
	},
	HiExampleCommit {
		#id : ''f6f253136e6621969cadc4099c1b599d1468b91d'',
		#comment : ''Fix IceMetacelloRepositoryAdapter to work with the loaded code commit instead of HEAD'',
		#parentIds : [
			''2fb92201151bd397c9d9b325aa068ab7dfa4c82b''
		]
	},
	HiExampleCommit {
		#id : ''2fb92201151bd397c9d9b325aa068ab7dfa4c82b'',
		#comment : ''Better management of baseline postloads'',
		#parentIds : [
			''ddb7aaddbd3453eb7f07a673748ab3506ad48315''
		]
	},
	HiExampleCommit {
		#id : ''7474e2513df95285c7158ca125a57942d660ad5e'',
		#comment : ''Merge pull request #787 from VincentBlondeau/windowsCI\n\nAdd Windows ci with Appveyor'',
		#parentIds : [
			''acd66a0b5cc22ffe1317434f9bc41b809b70571c'',
			''da697364d20e05b9e976a326e7b6d8589d28e720''
		]
	},
	HiExampleCommit {
		#id : ''ddb7aaddbd3453eb7f07a673748ab3506ad48315'',
		#comment : ''Merge pull request #831 from guillep/issue/823\n\nAdd Iceberg bootstrap'',
		#parentIds : [
			''dec483a56ae5e704eb7d7b3b68ff72807b3b7caa'',
			''687b4ec6502ba45db36b3698fa61fef0466795b3''
		]
	},
	HiExampleCommit {
		#id : ''687b4ec6502ba45db36b3698fa61fef0466795b3'',
		#comment : ''Reorganize lib git error dispatching'',
		#parentIds : [
			''0899d9295713050b48ac3412e61961096fde463a''
		]
	},
	HiExampleCommit {
		#id : ''0899d9295713050b48ac3412e61961096fde463a'',
		#comment : ''Add visitENOTFOUND: and exted LGit_GIT_ENOTFOUND to dispatch to it'',
		#parentIds : [
			''ef112a69205dbba5c9861efec654ba941f8eac64''
		]
	},
	HiExampleCommit {
		#id : ''ef112a69205dbba5c9861efec654ba941f8eac64'',
		#comment : ''Fix #823\n- add Iceberg bootstrap\n- execute bootstrap on load\n- Use revparse instead of revwalk to lookup for commits\n- Manage ENOTFOUND'',
		#parentIds : [
			''dec483a56ae5e704eb7d7b3b68ff72807b3b7caa''
		]
	},
	HiExampleCommit {
		#id : ''dec483a56ae5e704eb7d7b3b68ff72807b3b7caa'',
		#comment : ''Merge pull request #830 from pharo-vcs/issue/822\n\nBetter handling of not github remote urls'',
		#parentIds : [
			''b9c5edd4141d37c76fe711395287aa393cb6847c'',
			''473b301db5c114be4761e2a8d9745e54847d7478''
		]
	},
	HiExampleCommit {
		#id : ''473b301db5c114be4761e2a8d9745e54847d7478'',
		#comment : ''Fix for gitolite urls'',
		#parentIds : [
			''0facf423c9d388bdfd47bcde8705fd122f7a8ce4''
		]
	},
	HiExampleCommit {
		#id : ''0facf423c9d388bdfd47bcde8705fd122f7a8ce4'',
		#comment : ''Managing better alternative SSH syntax (git@host:path.git)\n- tons of tests to cover parsing'',
		#parentIds : [
			''d8ca197c685615cbd8a152b36e7a86bcb93722c1''
		]
	},
	HiExampleCommit {
		#id : ''d8ca197c685615cbd8a152b36e7a86bcb93722c1'',
		#comment : ''Simplify url parsing using ZnUrl'',
		#parentIds : [
			''ffe4735f1f173f24d3e95d052b7bc4eb1495cb68''
		]
	},
	HiExampleCommit {
		#id : ''b9c5edd4141d37c76fe711395287aa393cb6847c'',
		#comment : ''Fix #825'',
		#parentIds : [
			''074fba1d57c40b885d90f5b54970700c0363af72''
		]
	},
	HiExampleCommit {
		#id : ''ffe4735f1f173f24d3e95d052b7bc4eb1495cb68'',
		#comment : ''Fix #822\n - Changed url parsing regex\n - added tests for new edge cases'',
		#parentIds : [
			''074fba1d57c40b885d90f5b54970700c0363af72''
		]
	},
	HiExampleCommit {
		#id : ''074fba1d57c40b885d90f5b54970700c0363af72'',
		#comment : ''Fix #826\n - #push and #pushTo: should do nothing for unborn branches\n - added tests'',
		#parentIds : [
			''02b31b6fb41e36cda1ab0fb1097924b4dab904bc''
		]
	},
	HiExampleCommit {
		#id : ''02b31b6fb41e36cda1ab0fb1097924b4dab904bc'',
		#comment : ''clean IceTipVersionHistoryBrowser \nbut reuse the name (deprecated)  because calypso use it.'',
		#parentIds : [
			''9d8c1c7797453f2b94a63db1a6c6d4d4461e95ce''
		]
	},
	HiExampleCommit {
		#id : ''9d8c1c7797453f2b94a63db1a6c6d4d4461e95ce'',
		#comment : ''IceTipVersionHistoryBrowser working properly\nsome cleanups and comments too.'',
		#parentIds : [
			''b7eb18d4e89d080d38801d2e03924782d9becc96''
		]
	},
	HiExampleCommit {
		#id : ''b7eb18d4e89d080d38801d2e03924782d9becc96'',
		#comment : ''first version working'',
		#parentIds : [
			''acd66a0b5cc22ffe1317434f9bc41b809b70571c''
		]
	},
	HiExampleCommit {
		#id : ''acd66a0b5cc22ffe1317434f9bc41b809b70571c'',
		#comment : ''Updated version in readme'',
		#parentIds : [
			''f34a103a34866207c2e9d90f8e89d31494a38f78''
		]
	},
	HiExampleCommit {
		#id : ''f34a103a34866207c2e9d90f8e89d31494a38f78'',
		#comment : ''Fix #821\n- Extend commits to get properties from them\n- `IceRepositoryProperties new` should be correctly (lazily) initialized\n- make Filetree migration condition more robust'',
		#parentIds : [
			''eb0e5ef542e31dfb3186401d1fc99d0f22857677''
		]
	},
	HiExampleCommit {
		#id : ''eb0e5ef542e31dfb3186401d1fc99d0f22857677'',
		#comment : ''Merge pull request #820 from hellerve/patch-1\n\nTypo and grammar fixes in README'',
		#parentIds : [
			''cf10c1b3851fed3021226a9e11fa90294dfe3dda'',
			''a9776a539e261ef2acafba866caa1c1738fe03bb''
		]
	},
	HiExampleCommit {
		#id : ''a9776a539e261ef2acafba866caa1c1738fe03bb'',
		#comment : ''README: typo and grammar fixes'',
		#parentIds : [
			''cf10c1b3851fed3021226a9e11fa90294dfe3dda''
		]
	},
	HiExampleCommit {
		#id : ''cf10c1b3851fed3021226a9e11fa90294dfe3dda'',
		#comment : ''Load migration plugin by default\n'',
		#parentIds : [
			''ae043cf022a06cc19967e31452a0ef1c1e851209''
		]
	},
	HiExampleCommit {
		#id : ''ae043cf022a06cc19967e31452a0ef1c1e851209'',
		#comment : ''Merge 316152c15cb6e4c797c35fe87413a6c7f0b03a0b'',
		#parentIds : [
			''daad07adbee5add0c5a3d3b2b09e6a9423f32f4d'',
			''316152c15cb6e4c797c35fe87413a6c7f0b03a0b''
		]
	},
	HiExampleCommit {
		#id : ''daad07adbee5add0c5a3d3b2b09e6a9423f32f4d'',
		#comment : ''Merge pull request #819 from hellerve/patch-1\n\nFix tooltip typo in settings'',
		#parentIds : [
			''19deaf893c7c8ad5e56d86f10e549ec3a782d40c'',
			''5d936fa2aba271d29caf0ca67e0f262f7ad389ee''
		]
	},
	HiExampleCommit {
		#id : ''5d936fa2aba271d29caf0ca67e0f262f7ad389ee'',
		#comment : ''settingsOn: fix tooltip typo, github// -> github://'',
		#parentIds : [
			''969dcdab51e86a8f17e13e4645e852fefa03cd95''
		]
	},
	HiExampleCommit {
		#id : ''316152c15cb6e4c797c35fe87413a6c7f0b03a0b'',
		#comment : ''refactor out tests\nadd tests to baseline'',
		#parentIds : [
			''20f7fc6aa5beef24d1f8f6197aae6b388a6911f2''
		]
	},
	HiExampleCommit {
		#id : ''969dcdab51e86a8f17e13e4645e852fefa03cd95'',
		#comment : ''Merge pull request #817 from jecisc/addContributingMd\n\nAdd a CONTRIBUTING.md file pointing to the wiki'',
		#parentIds : [
			''84733c3f8874bd26b703411205b1bd3d60e45c47'',
			''a52935ae4ffbd4080ce32d190461b172c8b13a27''
		]
	},
	HiExampleCommit {
		#id : ''20f7fc6aa5beef24d1f8f6197aae6b388a6911f2'',
		#comment : ''- refactor out the convert action \n- adding  tests'',
		#parentIds : [
			''e18b240c538b4f52353719946300f9e3fb65f4a7''
		]
	},
	HiExampleCommit {
		#id : ''e18b240c538b4f52353719946300f9e3fb65f4a7'',
		#comment : ''cleaning a bit'',
		#parentIds : [
			''00dc50ba951e2696a2107e5e8bb6aaf3e0175201''
		]
	},
	HiExampleCommit {
		#id : ''a52935ae4ffbd4080ce32d190461b172c8b13a27'',
		#comment : ''Add a CONTRIBUTING.md file pointing to the wiki'',
		#parentIds : [
			''84733c3f8874bd26b703411205b1bd3d60e45c47''
		]
	},
	HiExampleCommit {
		#id : ''00dc50ba951e2696a2107e5e8bb6aaf3e0175201'',
		#comment : ''refactor '',
		#parentIds : [
			''b46d925e9f7ccaee5faa462e2c84d69d1195330a''
		]
	},
	HiExampleCommit {
		#id : ''b46d925e9f7ccaee5faa462e2c84d69d1195330a'',
		#comment : ''first version of migration plugin, kind of working but still incomplete.'',
		#parentIds : [
			''19deaf893c7c8ad5e56d86f10e549ec3a782d40c''
		]
	},
	HiExampleCommit {
		#id : ''84733c3f8874bd26b703411205b1bd3d60e45c47'',
		#comment : ''Merge pull request #815 from Ducasse/patch-1\n\nUpdate README.md'',
		#parentIds : [
			''71c0cf35286e818e650d17703946976f0eed8875'',
			''69e5b317db4c4d3ef7fd45a5141318aac7ab8346''
		]
	},
	HiExampleCommit {
		#id : ''69e5b317db4c4d3ef7fd45a5141318aac7ab8346'',
		#comment : ''Update README.md'',
		#parentIds : [
			''71c0cf35286e818e650d17703946976f0eed8875''
		]
	},
	HiExampleCommit {
		#id : ''19deaf893c7c8ad5e56d86f10e549ec3a782d40c'',
		#comment : ''Tests for #814'',
		#parentIds : [
			''12d978ad6f42e96dfffe5a1dc048d1b7f43ec56f''
		]
	},
	HiExampleCommit {
		#id : ''12d978ad6f42e96dfffe5a1dc048d1b7f43ec56f'',
		#comment : ''Fix #814'',
		#parentIds : [
			''bc29214658f578c319095ae1f019743ae666d354''
		]
	},
	HiExampleCommit {
		#id : ''bc29214658f578c319095ae1f019743ae666d354'',
		#comment : ''Merge 1e620b35263391873b2b31cc06db94356d27ebf7'',
		#parentIds : [
			''d454332efcad567dd33f12996b8f5bf9acccaa59'',
			''1e620b35263391873b2b31cc06db94356d27ebf7''
		]
	},
	HiExampleCommit {
		#id : ''1e620b35263391873b2b31cc06db94356d27ebf7'',
		#comment : ''Fix #807.\n- Add error dialog'',
		#parentIds : [
			''71c0cf35286e818e650d17703946976f0eed8875''
		]
	},
	HiExampleCommit {
		#id : ''d454332efcad567dd33f12996b8f5bf9acccaa59'',
		#comment : ''Fix #786'',
		#parentIds : [
			''1305e7751b94d3234a2f4f6db93c31c21a0c45a5''
		]
	},
	HiExampleCommit {
		#id : ''1305e7751b94d3234a2f4f6db93c31c21a0c45a5'',
		#comment : ''Fix #810'',
		#parentIds : [
			''89debf5b3482bfb0e8571993a712f090c0b05f7d''
		]
	},
	HiExampleCommit {
		#id : ''89debf5b3482bfb0e8571993a712f090c0b05f7d'',
		#comment : ''Merge pull request #779 from guillep/feature/64bits\n\nMake tests work on 64 bits'',
		#parentIds : [
			''edbb60170c1a13f8701d94cbacf0ab854e177b27'',
			''8d297b4f340b371c4f388b811a340edff0006189''
		]
	},
	HiExampleCommit {
		#id : ''edbb60170c1a13f8701d94cbacf0ab854e177b27'',
		#comment : ''Merge pull request #801 from jecisc/removeNotReferencedPackages\n\nRemove not referenced packages'',
		#parentIds : [
			''4b234af60e2bfd8c5ddaa4e8187edc23cbba3dd0'',
			''0d27c4239e6c8bb403ef365c31ddcd13387ec3cb''
		]
	},
	HiExampleCommit {
		#id : ''4b234af60e2bfd8c5ddaa4e8187edc23cbba3dd0'',
		#comment : ''Merge pull request #803 from jecisc/correctSomeLintInTests\n\nCorrect some lints in tests.'',
		#parentIds : [
			''febb0d3a011497cfdf06c086c3916387a6fd5e33'',
			''c817331e86fd131a4bfc32c98e497b3f70ecc29b''
		]
	},
	HiExampleCommit {
		#id : ''8d297b4f340b371c4f388b811a340edff0006189'',
		#comment : ''Point to OSSubprocess v1.0.0'',
		#parentIds : [
			''3b136af213d22aaa4083af7fa88a641e8f6a4a08''
		]
	},
	HiExampleCommit {
		#id : ''febb0d3a011497cfdf06c086c3916387a6fd5e33'',
		#comment : ''Merge pull request #808 from tesonep/issue/806\n\nIssue/806'',
		#parentIds : [
			''0f1b326ebc23237e773e9b8a7de98b3e3024464a'',
			''06111f9846511072bd0b45235c6184adb3456d1c''
		]
	},
	HiExampleCommit {
		#id : ''06111f9846511072bd0b45235c6184adb3456d1c'',
		#comment : ''Fixing the availability of repair action when is there is a missing subdirectory.'',
		#parentIds : [
			''0f1b326ebc23237e773e9b8a7de98b3e3024464a''
		]
	},
	HiExampleCommit {
		#id : ''c817331e86fd131a4bfc32c98e497b3f70ecc29b'',
		#comment : ''Correct some lints in tests.\n\n- Categorization\n- Remove unused variables\n- Remove methods equivalent to their super\n- Use #assert:equals: instead of #assert: and #='',
		#parentIds : [
			''0f1b326ebc23237e773e9b8a7de98b3e3024464a''
		]
	},
	HiExampleCommit {
		#id : ''0d27c4239e6c8bb403ef365c31ddcd13387ec3cb'',
		#comment : ''Remove not referenced packages'',
		#parentIds : [
			''0f1b326ebc23237e773e9b8a7de98b3e3024464a''
		]
	},
	HiExampleCommit {
		#id : ''0f1b326ebc23237e773e9b8a7de98b3e3024464a'',
		#comment : ''Merge pull request #795 from jecisc/addCommitMessageToHistory\n\nAdd commit message to History.'',
		#parentIds : [
			''0411c4c58fcd0feaabf2d3ca12706b742733e30b'',
			''a7739af99d5d8377ebc2329efae0e96e6d693ea2''
		]
	},
	HiExampleCommit {
		#id : ''0411c4c58fcd0feaabf2d3ca12706b742733e30b'',
		#comment : ''Commit package removal'',
		#parentIds : [
			''1e7aae0172b7afaa705b412501d856b5bce5f5fc''
		]
	},
	HiExampleCommit {
		#id : ''1e7aae0172b7afaa705b412501d856b5bce5f5fc'',
		#comment : ''Removed Iceberg-UI package'',
		#parentIds : [
			''c4c859a3cae4375a1ff26dc28833d0148f6a5a1e''
		]
	},
	HiExampleCommit {
		#id : ''c4c859a3cae4375a1ff26dc28833d0148f6a5a1e'',
		#comment : ''Move still useful extensions to Iceberg-TipUI'',
		#parentIds : [
			''e98501615f7cb92e66c4b3ae3d1cf68fd6e4bd60''
		]
	},
	HiExampleCommit {
		#id : ''a7739af99d5d8377ebc2329efae0e96e6d693ea2'',
		#comment : ''Merge branch \''dev-0.7\'' into addCommitMessageToHistory\n\n'',
		#parentIds : [
			''2f97cf4df246013bf44a66eb244992c5f2c4a674'',
			''e98501615f7cb92e66c4b3ae3d1cf68fd6e4bd60''
		]
	},
	HiExampleCommit {
		#id : ''e98501615f7cb92e66c4b3ae3d1cf68fd6e4bd60'',
		#comment : ''Recover IceExtendedVersionBrowser'',
		#parentIds : [
			''a3ae2e68411366d50fee0908769f69dc722cc6af''
		]
	},
	HiExampleCommit {
		#id : ''a3ae2e68411366d50fee0908769f69dc722cc6af'',
		#comment : ''Part of #725\n - Move Glamour extensions close to user\n - Remove seemingly unused IceDiffNode extensions'',
		#parentIds : [
			''232eb78be072f297e176ff03945f7cccad0b7a09''
		]
	},
	HiExampleCommit {
		#id : ''2f97cf4df246013bf44a66eb244992c5f2c4a674'',
		#comment : ''Add commit message to History.\n\nSee https://github.com/pharo-vcs/iceberg/issues/793'',
		#parentIds : [
			''35041cadf6ef5083d3e222d37e19a8e6e1471a7f''
		]
	},
	HiExampleCommit {
		#id : ''232eb78be072f297e176ff03945f7cccad0b7a09'',
		#comment : ''Part of #725\nMove classes required by history browser close to users'',
		#parentIds : [
			''57006fb631a2556b1fe09263eafabcb5e96434a2''
		]
	},
	HiExampleCommit {
		#id : ''57006fb631a2556b1fe09263eafabcb5e96434a2'',
		#comment : ''Part of  #725\n - move old glamour based browsers close to their only user (Github)\n - move merge UI morphs to IceTipUI package'',
		#parentIds : [
			''eefdfd825782e5e9c9338e805818b4d78f7f388a''
		]
	},
	HiExampleCommit {
		#id : ''eefdfd825782e5e9c9338e805818b4d78f7f388a'',
		#comment : ''Fix #794\nRename protocol utility to utilities'',
		#parentIds : [
			''35041cadf6ef5083d3e222d37e19a8e6e1471a7f''
		]
	},
	HiExampleCommit {
		#id : ''35041cadf6ef5083d3e222d37e19a8e6e1471a7f'',
		#comment : ''Merge pull request #792 from jecisc/cleanAndMovePlaintextCredentialsModel\n\nClean and move IceAskForPlaintextCredentialsModel'',
		#parentIds : [
			''325ec04421a5907893c57c26937c164a5f248266'',
			''b1e68d5f56bdfa80d96d74c20d2c365f3ddbfa72''
		]
	},
	HiExampleCommit {
		#id : ''325ec04421a5907893c57c26937c164a5f248266'',
		#comment : ''Fix #768\nRemove specific (wrong) management of diffs'',
		#parentIds : [
			''c94f2ba046c01431af01300245f1467a5abb434c''
		]
	},
	HiExampleCommit {
		#id : ''b1e68d5f56bdfa80d96d74c20d2c365f3ddbfa72'',
		#comment : ''Clean and move IceAskForPlaintextCredentialsModel\n\n- Remove two unused variables\n- Move the class to Iceberg-Plugin-Github since it\''s the only user\n- Rename the class to add "GitHub" in the name\n\nMaybe later this could be use for other plugins (Bitbucket/Gitlab) and we will need a "Iceberg-Plugin-AbstractHosting" package or something like that? But for now it\''s the only user.'',
		#parentIds : [
			''61f4ef98d5d6f5dff6b7edf66c457269972119a1''
		]
	},
	HiExampleCommit {
		#id : ''c94f2ba046c01431af01300245f1467a5abb434c'',
		#comment : ''Merge 61f4ef98d5d6f5dff6b7edf66c457269972119a1'',
		#parentIds : [
			''7b932f3ffc6209f7f3b0ef17462951fc88b2e58c'',
			''61f4ef98d5d6f5dff6b7edf66c457269972119a1''
		]
	},
	HiExampleCommit {
		#id : ''7b932f3ffc6209f7f3b0ef17462951fc88b2e58c'',
		#comment : ''Fix #785\n- add onUpgrade block\n- add conflict policy selection dialog\n- better message for ugrade policy selection dialog'',
		#parentIds : [
			''b773a903c6204e7d056efa2a27d31cd33260593c''
		]
	},
	HiExampleCommit {
		#id : ''61f4ef98d5d6f5dff6b7edf66c457269972119a1'',
		#comment : ''Merge pull request #791 from jecisc/cf_beginToRemoveOldUI\n\nBegin to remove old UI.'',
		#parentIds : [
			''b773a903c6204e7d056efa2a27d31cd33260593c'',
			''55152ccf0adc79d5a4e0e5ef9c992daab702a76d''
		]
	},
	HiExampleCommit {
		#id : ''b773a903c6204e7d056efa2a27d31cd33260593c'',
		#comment : ''Fix #784\nManaging in #pullFrom: if remote does not exist\nAdd tests'',
		#parentIds : [
			''69ff4f5ed4edaa793bb4dc4026c520d77e467656''
		]
	},
	HiExampleCommit {
		#id : ''da697364d20e05b9e976a326e7b6d8589d28e720'',
		#comment : ''Add appveyor badge'',
		#parentIds : [
			''59c9394d161929e5aa180278f315c25f4bba6f18''
		]
	},
	HiExampleCommit {
		#id : ''59c9394d161929e5aa180278f315c25f4bba6f18'',
		#comment : ''readme try2\n\n'',
		#parentIds : [
			''496a11038bbff1b56c1ed9c7f585972597f2ff20''
		]
	},
	HiExampleCommit {
		#id : ''496a11038bbff1b56c1ed9c7f585972597f2ff20'',
		#comment : ''add readme\n\n'',
		#parentIds : [
			''6e6f7cda12f07b82bb842ad05ea66be52645b902''
		]
	},
	HiExampleCommit {
		#id : ''6e6f7cda12f07b82bb842ad05ea66be52645b902'',
		#comment : ''add windows build\n\n\n'',
		#parentIds : [
			''71c0cf35286e818e650d17703946976f0eed8875''
		]
	},
	HiExampleCommit {
		#id : ''3b136af213d22aaa4083af7fa88a641e8f6a4a08'',
		#comment : ''Not allow 64 bit failures in travis ci'',
		#parentIds : [
			''d41794629d9ffc396bf939627449477f0ee04946''
		]
	},
	HiExampleCommit {
		#id : ''d41794629d9ffc396bf939627449477f0ee04946'',
		#comment : ''Try latest commit of ossubprocess'',
		#parentIds : [
			''69ff4f5ed4edaa793bb4dc4026c520d77e467656''
		]
	},
	HiExampleCommit {
		#id : ''69ff4f5ed4edaa793bb4dc4026c520d77e467656'',
		#comment : ''Removing snapshot reponsibility from IceSavedPackageVersion'',
		#parentIds : [
			''21fc06afb2d4279e6059aff6af63c6ee13e622b6''
		]
	},
	HiExampleCommit {
		#id : ''21fc06afb2d4279e6059aff6af63c6ee13e622b6'',
		#comment : ''Merge pull request #776 from dionisiydk/newCommander062\n\nUpgrade to Commander 0.6.2'',
		#parentIds : [
			''be380785caa055367bfb4d90cc5be1d34b779c18'',
			''35da1cda21c04444d12709950810f27f5f1cd966''
		]
	},
	HiExampleCommit {
		#id : ''35da1cda21c04444d12709950810f27f5f1cd966'',
		#comment : ''Now commapder is hosted under pharo-ide and there is new version 0.6.2'',
		#parentIds : [
			''be380785caa055367bfb4d90cc5be1d34b779c18''
		]
	},
	HiExampleCommit {
		#id : ''be380785caa055367bfb4d90cc5be1d34b779c18'',
		#comment : ''Merge pull request #766 from jecisc/addCopyCommandToHistoryWindow\n\nAdd copy SHA command to history window.'',
		#parentIds : [
			''4a89f29f95a21f4e5a6ef1f8231d0e722bb4b134'',
			''650dd441b07f4551a07d2af2e379e19a74e6aa37''
		]
	},
	HiExampleCommit {
		#id : ''650dd441b07f4551a07d2af2e379e19a74e6aa37'',
		#comment : ''Remove duplication and remane menu item name.'',
		#parentIds : [
			''731b5349cee5b6b8e8d7d8df689ed96e195cc31e''
		]
	},
	HiExampleCommit {
		#id : ''4a89f29f95a21f4e5a6ef1f8231d0e722bb4b134'',
		#comment : ''implement missing "remove package from repository"'',
		#parentIds : [
			''dd3700ca470bd551a9d6092c363fb02ead60aaa8''
		]
	},
	HiExampleCommit {
		#id : ''731b5349cee5b6b8e8d7d8df689ed96e195cc31e'',
		#comment : ''Add copy command to history window.\n\nFixes https://github.com/pharo-vcs/iceberg/issues/765'',
		#parentIds : [
			''dd3700ca470bd551a9d6092c363fb02ead60aaa8''
		]
	},
	HiExampleCommit {
		#id : ''71c0cf35286e818e650d17703946976f0eed8875'',
		#comment : ''Merge pull request #759 from pharo-vcs/dev-0.7\n\nRelease dev 0.7'',
		#parentIds : [
			''15f44153e874e16c01a442e30d040a23ced2b36d'',
			''dd3700ca470bd551a9d6092c363fb02ead60aaa8''
		]
	},
	HiExampleCommit {
		#id : ''dd3700ca470bd551a9d6092c363fb02ead60aaa8'',
		#comment : ''Fix default action for upgrades and conflicts'',
		#parentIds : [
			''b2e28c98f999dcfa5dff7c6416763a54916cb6d7''
		]
	},
	HiExampleCommit {
		#id : ''b2e28c98f999dcfa5dff7c6416763a54916cb6d7'',
		#comment : ''Fix #625\n- adapting project registrations\n- locked should rerturn false\n- extended metacello integration tests'',
		#parentIds : [
			''6213965215416b05de6ae271d56c84f4c25cf1a5''
		]
	},
	HiExampleCommit {
		#id : ''6213965215416b05de6ae271d56c84f4c25cf1a5'',
		#comment : ''Fix #758\nrecover repositorys setter'',
		#parentIds : [
			''c348b65d6d15da9b0fe789343d8c893f50c7b5db''
		]
	},
	HiExampleCommit {
		#id : ''c348b65d6d15da9b0fe789343d8c893f50c7b5db'',
		#comment : ''Merge pull request #757 from jecisc/cf_correctUseOfEmphasisInPharo6\n\nAdd compatibility methods for Pharo 6.1'',
		#parentIds : [
			''cef6f7d9b98561cd05713037c836b89b42dbed86'',
			''286e841118215e3dcd61b57c267df4c1c2da1573''
		]
	},
	HiExampleCommit {
		#id : ''286e841118215e3dcd61b57c267df4c1c2da1573'',
		#comment : ''Add compatibility methods for Pharo 6.1\n\nFixes https://github.com/pharo-vcs/iceberg/issues/756'',
		#parentIds : [
			''cef6f7d9b98561cd05713037c836b89b42dbed86''
		]
	},
	HiExampleCommit {
		#id : ''cef6f7d9b98561cd05713037c836b89b42dbed86'',
		#comment : ''Fix clone tests that now should expect ice exceptions'',
		#parentIds : [
			''112f5fa5d4f2ef5f0f7b75c5fbaebc5605173dfd''
		]
	},
	HiExampleCommit {
		#id : ''112f5fa5d4f2ef5f0f7b75c5fbaebc5605173dfd'',
		#comment : ''Better feedback for #688'',
		#parentIds : [
			''64474789b6961740e34b67360d2493e568cc00e6''
		]
	},
	HiExampleCommit {
		#id : ''64474789b6961740e34b67360d2493e568cc00e6'',
		#comment : ''Cleanup old plugins'',
		#parentIds : [
			''e94082ca2625752735e46738606027a7c6cbc3f4''
		]
	},
	HiExampleCommit {
		#id : ''e94082ca2625752735e46738606027a7c6cbc3f4'',
		#comment : ''Merge pull request #749 from tesonep/issue/748\n\nAdding repair actions to the code subdirectory missing problem.'',
		#parentIds : [
			''d49d440bc3b65d19b10184f1963964b3dd579c2d'',
			''c61eba50028eddf61d167371c2e3fc11573d69c9''
		]
	},
	HiExampleCommit {
		#id : ''d49d440bc3b65d19b10184f1963964b3dd579c2d'',
		#comment : ''Merge 3b0124713b6bd1f8bd167a79e6e8bfd98604b1e0'',
		#parentIds : [
			''82816581354366d5ef825ba8e8e2c5a25309fdbe'',
			''3b0124713b6bd1f8bd167a79e6e8bfd98604b1e0''
		]
	},
	HiExampleCommit {
		#id : ''82816581354366d5ef825ba8e8e2c5a25309fdbe'',
		#comment : ''Fix #655\nPrefixing spec additions to extract in components'',
		#parentIds : [
			''e10ab94d4e8ecb518f9bd1c77a3544752acf45d8''
		]
	},
	HiExampleCommit {
		#id : ''3b0124713b6bd1f8bd167a79e6e8bfd98604b1e0'',
		#comment : ''Merge pull request #755 from tesonep/issue/750\n\nExtracting the URL label behavior as a component that we can reuse.'',
		#parentIds : [
			''e10ab94d4e8ecb518f9bd1c77a3544752acf45d8'',
			''c0c01f9b60a6ba7d9bb83ad9d29ecd64ff574649''
		]
	},
	HiExampleCommit {
		#id : ''e10ab94d4e8ecb518f9bd1c77a3544752acf45d8'',
		#comment : ''Merge 13aa237e95a1e8e95fe96010d88e4adf4cbd217e'',
		#parentIds : [
			''c127995e69deae907c8ebc7fe9988f525c0b69b9'',
			''13aa237e95a1e8e95fe96010d88e4adf4cbd217e''
		]
	},
	HiExampleCommit {
		#id : ''c127995e69deae907c8ebc7fe9988f525c0b69b9'',
		#comment : ''Fix #703\n - extract pushing into push action\n - do not let generic libgit errors pass'',
		#parentIds : [
			''4b96fd9327c1ac2a472c6d2ab39cbcc9d495c01a''
		]
	},
	HiExampleCommit {
		#id : ''c0c01f9b60a6ba7d9bb83ad9d29ecd64ff574649'',
		#comment : ''Extracting the URL label behavior as a component that we can reuse.'',
		#parentIds : [
			''553da57205370816485d9246a4d63c88abcddec7''
		]
	},
	HiExampleCommit {
		#id : ''13aa237e95a1e8e95fe96010d88e4adf4cbd217e'',
		#comment : ''Merge pull request #754 from tesonep/issue/750\n\nShowing and coping the Commit ID'',
		#parentIds : [
			''4b96fd9327c1ac2a472c6d2ab39cbcc9d495c01a'',
			''553da57205370816485d9246a4d63c88abcddec7''
		]
	},
	HiExampleCommit {
		#id : ''4b96fd9327c1ac2a472c6d2ab39cbcc9d495c01a'',
		#comment : ''Enhance general focus order in option dialogs\n'',
		#parentIds : [
			''0a46e245e14e3cd86dd5d2fca54127188edc3b0a''
		]
	},
	HiExampleCommit {
		#id : ''553da57205370816485d9246a4d63c88abcddec7'',
		#comment : ''Adding the short commit ID to the package window.\nAdding an option to copy the commit ID.\nAdding behavior to copy the commit ID when clicking in it.'',
		#parentIds : [
			''2bab82f52908a06df381ad161663cb9eaa2b9d84''
		]
	},
	HiExampleCommit {
		#id : ''0a46e245e14e3cd86dd5d2fca54127188edc3b0a'',
		#comment : ''Fix #753\n - rename Fogbugz -> Manuscript\n - Restore space panel\n - make menu option behave as the checkout branch button'',
		#parentIds : [
			''859c5e3d114029d5a0416b2f43c8b021e27115e6''
		]
	},
	HiExampleCommit {
		#id : ''859c5e3d114029d5a0416b2f43c8b021e27115e6'',
		#comment : ''Fix #664\n- Cleanup of packages should use package name and not repository name\n- avoid RPackage cache during cleanup'',
		#parentIds : [
			''2bab82f52908a06df381ad161663cb9eaa2b9d84''
		]
	},
	HiExampleCommit {
		#id : ''2bab82f52908a06df381ad161663cb9eaa2b9d84'',
		#comment : ''new SSH tests'',
		#parentIds : [
			''0c3b165531ba1fc7a648346fd4b1d5d50c96ee03''
		]
	},
	HiExampleCommit {
		#id : ''0c3b165531ba1fc7a648346fd4b1d5d50c96ee03'',
		#comment : ''Merge cd86282020016c8346a0097761e392894d7bd32a'',
		#parentIds : [
			''7c44a9ce38c52ecaf261e207f3fc116ccde0ff8d'',
			''cd86282020016c8346a0097761e392894d7bd32a''
		]
	},
	HiExampleCommit {
		#id : ''7c44a9ce38c52ecaf261e207f3fc116ccde0ff8d'',
		#comment : ''Wrap all libgit accesses with exception handling.\nAdd architectural rules to validate libgit access'',
		#parentIds : [
			''1ca6b1a6579f15f22c7c6ff4ef287b98ff2b9fd3''
		]
	},
	HiExampleCommit {
		#id : ''cd86282020016c8346a0097761e392894d7bd32a'',
		#comment : ''Merge 1ca6b1a6579f15f22c7c6ff4ef287b98ff2b9fd3'',
		#parentIds : [
			''ea67cc841d6e08aed194800bcb08100d93cb9ff9'',
			''1ca6b1a6579f15f22c7c6ff4ef287b98ff2b9fd3''
		]
	},
	HiExampleCommit {
		#id : ''c61eba50028eddf61d167371c2e3fc11573d69c9'',
		#comment : ''Creating new src directory in tonel format.'',
		#parentIds : [
			''ae5130ebb52898bb178e3a5809b840b1d8e206b0''
		]
	},
	HiExampleCommit {
		#id : ''ae5130ebb52898bb178e3a5809b840b1d8e206b0'',
		#comment : ''Merge 1ca6b1a6579f15f22c7c6ff4ef287b98ff2b9fd3'',
		#parentIds : [
			''5c98da21652c03578bedacc222623b6f1e6e4a9d'',
			''1ca6b1a6579f15f22c7c6ff4ef287b98ff2b9fd3''
		]
	},
	HiExampleCommit {
		#id : ''5c98da21652c03578bedacc222623b6f1e6e4a9d'',
		#comment : ''Adding repair actions to the code subdirectory missing problem.\nFixes #748'',
		#parentIds : [
			''c647d0f8356e8594bcc667b20f05a2d557ef35ae''
		]
	},
	HiExampleCommit {
		#id : ''1ca6b1a6579f15f22c7c6ff4ef287b98ff2b9fd3'',
		#comment : ''Merge pull request #747 from tesonep/issue/743\n\nCloning a Git repository should change the path with the project name.'',
		#parentIds : [
			''c647d0f8356e8594bcc667b20f05a2d557ef35ae'',
			''76e634764d35a63d6451e61a64c0f86bdbf22123''
		]
	},
	HiExampleCommit {
		#id : ''76e634764d35a63d6451e61a64c0f86bdbf22123'',
		#comment : ''Adding the append of the project name to the location, this was done in the other panels but not in the \''Clone git repository\'''',
		#parentIds : [
			''c647d0f8356e8594bcc667b20f05a2d557ef35ae''
		]
	},
	HiExampleCommit {
		#id : ''c647d0f8356e8594bcc667b20f05a2d557ef35ae'',
		#comment : ''Fix #742\nCapture error during clone\nAdd tests'',
		#parentIds : [
			''784ff6fe9b09c0aaab28a645bcab6c275d3ae051''
		]
	},
	HiExampleCommit {
		#id : ''784ff6fe9b09c0aaab28a645bcab6c275d3ae051'',
		#comment : ''Fix #741'',
		#parentIds : [
			''f66806f3bd0ac4a3ad7b11d55765f0fc547a4a19''
		]
	},
	HiExampleCommit {
		#id : ''f66806f3bd0ac4a3ad7b11d55765f0fc547a4a19'',
		#comment : ''Merge 41b265d6799b67e063605f060bae862731beac06'',
		#parentIds : [
			''ae8938463eba71a8cf5669ce32cff645d8971bf8'',
			''41b265d6799b67e063605f060bae862731beac06''
		]
	},
	HiExampleCommit {
		#id : ''ae8938463eba71a8cf5669ce32cff645d8971bf8'',
		#comment : ''Test delete and avoid deleting empty locations'',
		#parentIds : [
			''1eebd912667bf905e82fa301b82fdd46242f15e6''
		]
	},
	HiExampleCommit {
		#id : ''41b265d6799b67e063605f060bae862731beac06'',
		#comment : ''Fix #746\nDo not allow Pharo6.1 builds to fail\n'',
		#parentIds : [
			''1eebd912667bf905e82fa301b82fdd46242f15e6''
		]
	},
	HiExampleCommit {
		#id : ''1eebd912667bf905e82fa301b82fdd46242f15e6'',
		#comment : ''use same script as in readme\n'',
		#parentIds : [
			''bde9d38ad674c3a2713f6aeb557dc3886a864833''
		]
	},
	HiExampleCommit {
		#id : ''bde9d38ad674c3a2713f6aeb557dc3886a864833'',
		#comment : ''Add Pharo 6 extension package to unload'',
		#parentIds : [
			''9e6e7c614e3de9918deac10c9e8454d1864db3d9''
		]
	},
	HiExampleCommit {
		#id : ''9e6e7c614e3de9918deac10c9e8454d1864db3d9'',
		#comment : ''Fix package name typo'',
		#parentIds : [
			''795b6cb705cd1005fc180a6cd59c4e07aaced32a''
		]
	},
	HiExampleCommit {
		#id : ''795b6cb705cd1005fc180a6cd59c4e07aaced32a'',
		#comment : ''Add Pharo6 package in requirements'',
		#parentIds : [
			''55af53226fddc7fd5c35f8934a991321dd82620d''
		]
	},
	HiExampleCommit {
		#id : ''55af53226fddc7fd5c35f8934a991321dd82620d'',
		#comment : ''Cleanup some global variables that seem dirty on CI updates'',
		#parentIds : [
			''6f97b90e16c3ab58ccfb5076a48b893025fbfddd''
		]
	},
	HiExampleCommit {
		#id : ''6f97b90e16c3ab58ccfb5076a48b893025fbfddd'',
		#comment : ''Merge branch \''dev-0.7\'' of github.com:pharo-vcs/iceberg into dev-0.7\n'',
		#parentIds : [
			''9755cddba70e135453e7c982d5934512e44ec845'',
			''1fd6a46121290e7635fe523440e9acac39defe9b''
		]
	},
	HiExampleCommit {
		#id : ''9755cddba70e135453e7c982d5934512e44ec845'',
		#comment : ''Merge branch \''master\'' into dev-0.7\n'',
		#parentIds : [
			''a2f4368dbc8085571d5616625c2992f02ef6d7da'',
			''05d15893fbb4011994a636a7bb782408ee6e7248''
		]
	},
	HiExampleCommit {
		#id : ''1fd6a46121290e7635fe523440e9acac39defe9b'',
		#comment : ''Adding debug information'',
		#parentIds : [
			''a2f4368dbc8085571d5616625c2992f02ef6d7da''
		]
	},
	HiExampleCommit {
		#id : ''05d15893fbb4011994a636a7bb782408ee6e7248'',
		#comment : ''Fix #735\n- better manage root paths'',
		#parentIds : [
			''62f4ad60aa86675b1b28eb7f752f5150d6da9bec''
		]
	},
	HiExampleCommit {
		#id : ''62f4ad60aa86675b1b28eb7f752f5150d6da9bec'',
		#comment : ''Make teardown more robust for pharo 6.1'',
		#parentIds : [
			''bafc9ee19c5e00422c59bebd47c5c5af2082d7a5''
		]
	},
	HiExampleCommit {
		#id : ''bafc9ee19c5e00422c59bebd47c5c5af2082d7a5'',
		#comment : ''Add deny:equals: for pharo6 compatibility'',
		#parentIds : [
			''15f44153e874e16c01a442e30d040a23ced2b36d''
		]
	},
	HiExampleCommit {
		#id : ''a2f4368dbc8085571d5616625c2992f02ef6d7da'',
		#comment : ''Merge pull request #733 from pharo-vcs/add-license-1\n\nAdd license file'',
		#parentIds : [
			''525959bec14b095f1079eb062d2de416b0ebf81e'',
			''a5abc57c0d6fa25748cb8bcbb38823368ca3ce3f''
		]
	},
	HiExampleCommit {
		#id : ''525959bec14b095f1079eb062d2de416b0ebf81e'',
		#comment : ''Merge pull request #740 from jecisc/cf_ImproveFogbuzPanel\n\nFogbugz panel: Improve label, focus order and layout'',
		#parentIds : [
			''35c39662306772437f21888f3ad133c8680f6087'',
			''28faf23cf63403c7f84fc0fa70f66a9f9037933f''
		]
	},
	HiExampleCommit {
		#id : ''28faf23cf63403c7f84fc0fa70f66a9f9037933f'',
		#comment : ''Refactor fix to get less duplication'',
		#parentIds : [
			''31fb47b6a69799bbffad9a239ea7bdd20d618fb7''
		]
	},
	HiExampleCommit {
		#id : ''31fb47b6a69799bbffad9a239ea7bdd20d618fb7'',
		#comment : ''Fogbugz panel: Improve label, focus order and layout\n\n- Rename label from FogBugz issue to Pharo issue Number\n- The focus order now works right\n- Redefine the extent of the window\n\nFixes https://github.com/pharo-vcs/iceberg/issues/739'',
		#parentIds : [
			''35c39662306772437f21888f3ad133c8680f6087''
		]
	},
	HiExampleCommit {
		#id : ''15f44153e874e16c01a442e30d040a23ced2b36d'',
		#comment : ''Merge pull request #736 from pharo-vcs/dev-0.7\n\nFix failing Pharo tests due to bad recategorizations'',
		#parentIds : [
			''b3c33f66e187059ea2bc1a7386bd17ba436031e5'',
			''35c39662306772437f21888f3ad133c8680f6087''
		]
	},
	HiExampleCommit {
		#id : ''35c39662306772437f21888f3ad133c8680f6087'',
		#comment : ''Fix failing Pharo tests due to bad recategorizations'',
		#parentIds : [
			''70adda00e5a97094a7a0efd3923fa7aefe91b28c''
		]
	},
	HiExampleCommit {
		#id : ''55152ccf0adc79d5a4e0e5ef9c992daab702a76d'',
		#comment : ''Begin to remove old UI.\n\nPart of https://github.com/pharo-vcs/iceberg/issues/725\n\nThis is far from been done but it remove a lot of unused classes and move some stuff used by the new UI to its package.'',
		#parentIds : [
			''70adda00e5a97094a7a0efd3923fa7aefe91b28c''
		]
	},
	HiExampleCommit {
		#id : ''a5abc57c0d6fa25748cb8bcbb38823368ca3ce3f'',
		#comment : ''Create LICENSE'',
		#parentIds : [
			''b3c33f66e187059ea2bc1a7386bd17ba436031e5''
		]
	},
	HiExampleCommit {
		#id : ''b3c33f66e187059ea2bc1a7386bd17ba436031e5'',
		#comment : ''Merge pull request #732 from pharo-vcs/dev-0.7\n\nRelease 0.7.4'',
		#parentIds : [
			''53bfa5e98b53c980b76c38e29c64581bcf2fc3c0'',
			''70adda00e5a97094a7a0efd3923fa7aefe91b28c''
		]
	},
	HiExampleCommit {
		#id : ''70adda00e5a97094a7a0efd3923fa7aefe91b28c'',
		#comment : ''Merge 12bfdcb8f14027e4ea019ee6a35f732db4e6ef4f'',
		#parentIds : [
			''9dee83fe1eb438c1a981033f990bc6e0668a0315'',
			''12bfdcb8f14027e4ea019ee6a35f732db4e6ef4f''
		]
	},
	HiExampleCommit {
		#id : ''9dee83fe1eb438c1a981033f990bc6e0668a0315'',
		#comment : ''Avoid showing discard changes on unknown commits'',
		#parentIds : [
			''4889447d8295a43904d021bb7753a46af8dce9a5''
		]
	},
	HiExampleCommit {
		#id : ''53bfa5e98b53c980b76c38e29c64581bcf2fc3c0'',
		#comment : ''Merge pull request #731 from pharo-vcs/dev-0.7\n\nPrepare Release 0.7.4'',
		#parentIds : [
			''78a671f1ef1231acef4d39097339350dc2a090c7'',
			''12bfdcb8f14027e4ea019ee6a35f732db4e6ef4f''
		]
	},
	HiExampleCommit {
		#id : ''12bfdcb8f14027e4ea019ee6a35f732db4e6ef4f'',
		#comment : ''Merge pull request #729 from pharo-vcs/ci/moreci\n\nUpdate .travis.yml to test Pharo 6 and 64 bits'',
		#parentIds : [
			''4889447d8295a43904d021bb7753a46af8dce9a5'',
			''8522146492befe570d1f70331923ffd922cde77c''
		]
	},
	HiExampleCommit {
		#id : ''8522146492befe570d1f70331923ffd922cde77c'',
		#comment : ''Update .travis.yml'',
		#parentIds : [
			''da681e3f2e71b889df52e42f01787185dc855592''
		]
	},
	HiExampleCommit {
		#id : ''da681e3f2e71b889df52e42f01787185dc855592'',
		#comment : ''Add smalltalk key to allow failures matrix'',
		#parentIds : [
			''5f04f9d4a19ff386c082eb2e3a8be262e3928ead''
		]
	},
	HiExampleCommit {
		#id : ''4889447d8295a43904d021bb7753a46af8dce9a5'',
		#comment : ''Merge afb586f78c9655d9f62373913e65a215cf42d346'',
		#parentIds : [
			''2c23579d181f17d5f7f80b9404fd48cb2315ace0'',
			''afb586f78c9655d9f62373913e65a215cf42d346''
		]
	},
	HiExampleCommit {
		#id : ''2c23579d181f17d5f7f80b9404fd48cb2315ace0'',
		#comment : ''Support "Esc" key when pull is asked'',
		#parentIds : [
			''5357ff396de3d9bd2fa7bb3d64907c1141dcf1cf''
		]
	},
	HiExampleCommit {
		#id : ''5357ff396de3d9bd2fa7bb3d64907c1141dcf1cf'',
		#comment : ''Make tests pass'',
		#parentIds : [
			''9cdfc5a6d75738461e6de7cc5f8b94d12533f951''
		]
	},
	HiExampleCommit {
		#id : ''9cdfc5a6d75738461e6de7cc5f8b94d12533f951'',
		#comment : ''Added revert tests'',
		#parentIds : [
			''8a7c1cfc5c0fff08c7d79a4425683199ab8d83d0''
		]
	},
	HiExampleCommit {
		#id : ''5f04f9d4a19ff386c082eb2e3a8be262e3928ead'',
		#comment : ''Allow failures'',
		#parentIds : [
			''116f64cb5eef67874cb03195140549ca0b0b2911''
		]
	},
	HiExampleCommit {
		#id : ''afb586f78c9655d9f62373913e65a215cf42d346'',
		#comment : ''Merge pull request #727 from jecisc/cf_correctLoadingOfTonelProjectsInPharo6\n\nAdapt properties reading of Tonel projects for Pharo 6.1.'',
		#parentIds : [
			''ef806b9004863798ff049941b482ba2b52e11fe3'',
			''6aa8194e7b5799203483b34bc1319db216043915''
		]
	},
	HiExampleCommit {
		#id : ''ef806b9004863798ff049941b482ba2b52e11fe3'',
		#comment : ''Merge pull request #728 from jecisc/cf_askConfirmationOnFetchAll\n\nAsk confirmation on fetch all action'',
		#parentIds : [
			''1cdfe13b5f738d52f9ea7aee165e4dcb662aed43'',
			''811fdf2baa9ac6a55da692bc1a43a18301358552''
		]
	},
	HiExampleCommit {
		#id : ''811fdf2baa9ac6a55da692bc1a43a18301358552'',
		#comment : ''When the user try to fetch for all repositories ask confirmation since it car takes some time.\n\nFixes https://github.com/pharo-vcs/iceberg/issues/726'',
		#parentIds : [
			''1cdfe13b5f738d52f9ea7aee165e4dcb662aed43''
		]
	},
	HiExampleCommit {
		#id : ''8a7c1cfc5c0fff08c7d79a4425683199ab8d83d0'',
		#comment : ''Make tests pass'',
		#parentIds : [
			''e9fcc8bd686ab915009eb17d708cab8095d45ffa''
		]
	},
	HiExampleCommit {
		#id : ''6aa8194e7b5799203483b34bc1319db216043915'',
		#comment : ''Adapt properties reading of Tonel projects for Pharo 6.1.\n\nFixes https://github.com/pharo-vcs/iceberg/issues/669'',
		#parentIds : [
			''1cdfe13b5f738d52f9ea7aee165e4dcb662aed43''
		]
	},
	HiExampleCommit {
		#id : ''e9fcc8bd686ab915009eb17d708cab8095d45ffa'',
		#comment : ''Removal of packages with tests'',
		#parentIds : [
			''8ebc68ad76b897df5de436275f648f5a5edb896e''
		]
	},
	HiExampleCommit {
		#id : ''8ebc68ad76b897df5de436275f648f5a5edb896e'',
		#comment : ''Detect package deletion'',
		#parentIds : [
			''86f966d53c40cb941cd099d60b265ed7c335ddce''
		]
	},
	HiExampleCommit {
		#id : ''86f966d53c40cb941cd099d60b265ed7c335ddce'',
		#comment : ''Merge 1cdfe13b5f738d52f9ea7aee165e4dcb662aed43'',
		#parentIds : [
			''61304aec3536ac5df8a86341e6e9eaa84fc4aae4'',
			''1cdfe13b5f738d52f9ea7aee165e4dcb662aed43''
		]
	},
	HiExampleCommit {
		#id : ''61304aec3536ac5df8a86341e6e9eaa84fc4aae4'',
		#comment : ''Adding a remove package dialog'',
		#parentIds : [
			''65947f47a573fdf8b9b6ab134340e7efc7e23092''
		]
	},
	HiExampleCommit {
		#id : ''1cdfe13b5f738d52f9ea7aee165e4dcb662aed43'',
		#comment : ''Fix #695\n - add url builder + tests\n - extended tip provider dialog with protocol support'',
		#parentIds : [
			''65947f47a573fdf8b9b6ab134340e7efc7e23092''
		]
	},
	HiExampleCommit {
		#id : ''65947f47a573fdf8b9b6ab134340e7efc7e23092'',
		#comment : ''Simplify revert:\n - reverting is the application of an inverted tree\n - revert visitor is not needed anymore\n - logic is now shared between memory and git backends, ring and normal working copies\n - added tests'',
		#parentIds : [
			''f38aeef9506384e4af6c135c612451a93aa8347e''
		]
	},
	HiExampleCommit {
		#id : ''f38aeef9506384e4af6c135c612451a93aa8347e'',
		#comment : ''Cleanups'',
		#parentIds : [
			''6f66f426ed1ae082e69e8d0247f470d0dd20f8d8''
		]
	},
	HiExampleCommit {
		#id : ''6f66f426ed1ae082e69e8d0247f470d0dd20f8d8'',
		#comment : ''Testing #690'',
		#parentIds : [
			''53d1339e839e96e7ec1282eb7f25c23b198a3248''
		]
	},
	HiExampleCommit {
		#id : ''53d1339e839e96e7ec1282eb7f25c23b198a3248'',
		#comment : ''Refactoring diff to have a single definition of working copy diff'',
		#parentIds : [
			''e3cae04cb243a8e45baa91f1fe58631e5ac291a7''
		]
	},
	HiExampleCommit {
		#id : ''e3cae04cb243a8e45baa91f1fe58631e5ac291a7'',
		#comment : ''Merge 146eaf53f01af03c9782466f3b8b3930da62f405'',
		#parentIds : [
			''5d57178850ca99a05043762aecf30824ca4572bf'',
			''146eaf53f01af03c9782466f3b8b3930da62f405''
		]
	},
	HiExampleCommit {
		#id : ''5d57178850ca99a05043762aecf30824ca4572bf'',
		#comment : ''Fix #690\nMake diff to revert against referenceCommit'',
		#parentIds : [
			''22934211e537e6778420b929b987e23578f22138''
		]
	},
	HiExampleCommit {
		#id : ''146eaf53f01af03c9782466f3b8b3930da62f405'',
		#comment : ''Merge pull request #724 from jecisc/cf_changeFocusOrderOfCommitWindow\n\nChange focus order of commit window'',
		#parentIds : [
			''0affe3a481b7995a67fe02a6624e6e7a3cfb0de5'',
			''baa5bc3d68b67a6b057e6252fe1c505654b3f7f3''
		]
	},
	HiExampleCommit {
		#id : ''0affe3a481b7995a67fe02a6624e6e7a3cfb0de5'',
		#comment : ''Allowing search in the trees used in Iceberg\nFixes #719'',
		#parentIds : [
			''6076b39dd21d715a0c36bca31587b981dc5963f1''
		]
	},
	HiExampleCommit {
		#id : ''baa5bc3d68b67a6b057e6252fe1c505654b3f7f3'',
		#comment : ''Change focus order of commit window\n\nInstead of selecting the diff view first, select the commit text area.\n\nFixes: https://github.com/pharo-vcs/iceberg/issues/720'',
		#parentIds : [
			''ba2841fd20e12fbf0bdfa6d96e214d3cda4a55e3''
		]
	},
	HiExampleCommit {
		#id : ''6076b39dd21d715a0c36bca31587b981dc5963f1'',
		#comment : ''When a class instance variable is modified, it was not propagated.\n\nFixes #714\nAdding tests'',
		#parentIds : [
			''3a68c2f415e8730c701b7a14f2818175118cb3f4''
		]
	},
	HiExampleCommit {
		#id : ''3a68c2f415e8730c701b7a14f2818175118cb3f4'',
		#comment : ''Skipping SSH test in the CI'',
		#parentIds : [
			''a2876861fc17583a9b8cc83e4d1753ab128fa2a0''
		]
	},
	HiExampleCommit {
		#id : ''a2876861fc17583a9b8cc83e4d1753ab128fa2a0'',
		#comment : ''Fixes #721\nHandling better the cloning of repositories.'',
		#parentIds : [
			''ba2841fd20e12fbf0bdfa6d96e214d3cda4a55e3''
		]
	},
	HiExampleCommit {
		#id : ''ba2841fd20e12fbf0bdfa6d96e214d3cda4a55e3'',
		#comment : ''Fixing the tests of Git config.\nRenaming some tests.'',
		#parentIds : [
			''8537a7c75584913c5b893c92e3d81d00fd401fca''
		]
	},
	HiExampleCommit {
		#id : ''8537a7c75584913c5b893c92e3d81d00fd401fca'',
		#comment : ''Updating to new version of Libgit.'',
		#parentIds : [
			''03a34db30730a06ee1c37a295c9c7cd1ca7bae2c''
		]
	},
	HiExampleCommit {
		#id : ''03a34db30730a06ee1c37a295c9c7cd1ca7bae2c'',
		#comment : ''Move method to test package'',
		#parentIds : [
			''2ad40c66534ab0861711b47e6e61d14588888c35''
		]
	},
	HiExampleCommit {
		#id : ''2ad40c66534ab0861711b47e6e61d14588888c35'',
		#comment : ''Fixing broken tests.'',
		#parentIds : [
			''22934211e537e6778420b929b987e23578f22138''
		]
	},
	HiExampleCommit {
		#id : ''22934211e537e6778420b929b987e23578f22138'',
		#comment : ''Use a root node to diff'',
		#parentIds : [
			''5197c57c3be262be8457315fdf5f699e18d38ec0''
		]
	},
	HiExampleCommit {
		#id : ''5197c57c3be262be8457315fdf5f699e18d38ec0'',
		#comment : ''Restore change removed due to bad merge'',
		#parentIds : [
			''bf19d1911843f94b4fc807fc53296dbb67353464''
		]
	},
	HiExampleCommit {
		#id : ''bf19d1911843f94b4fc807fc53296dbb67353464'',
		#comment : ''Fix #714\n - take into account isolated class side changes\n - added tests for it'',
		#parentIds : [
			''459cd4208e4f2182305cd3769910aa4b5cdea7ad''
		]
	},
	HiExampleCommit {
		#id : ''459cd4208e4f2182305cd3769910aa4b5cdea7ad'',
		#comment : ''Fix #716\n - better handling of missing repositories\n - added metacello integration tests'',
		#parentIds : [
			''0a178a213e4774ef409f6df925f1799fb450b7df''
		]
	},
	HiExampleCommit {
		#id : ''0a178a213e4774ef409f6df925f1799fb450b7df'',
		#comment : ''Merge pull request #715 from jecisc/cf_addCommentAboutRecompilation\n\nAdd a comment in the README about obsolete classes'',
		#parentIds : [
			''163f600b9854bb7ae9fb127d7df2bab96d4f50f5'',
			''55510b31db672e1e7ca8f21b872c98f10dffdf41''
		]
	},
	HiExampleCommit {
		#id : ''163f600b9854bb7ae9fb127d7df2bab96d4f50f5'',
		#comment : ''Fix #709, #673, #674\nIterating on the Merge view\n - Icons revised\n - selection revised\n - revised order of changes in diff\n - remove unused arrow in the middle of merge view'',
		#parentIds : [
			''de1dd567e5269f7c91b0a5293839d1809dde7e2f''
		]
	},
	HiExampleCommit {
		#id : ''de1dd567e5269f7c91b0a5293839d1809dde7e2f'',
		#comment : ''Fixes #701\nExtracting Merge logic into a reusable command.\nModifying the Merge preview to only show new elements.\nAdding tests'',
		#parentIds : [
			''b5ab800723a1405b149f35396e9bf857a8d27324''
		]
	},
	HiExampleCommit {
		#id : ''b5ab800723a1405b149f35396e9bf857a8d27324'',
		#comment : ''Merge d49ca0e3b75b0039cfb64efb0885eed909a36bcf'',
		#parentIds : [
			''ecf29a507e626365d52d4f574d88284f55586a1e'',
			''d49ca0e3b75b0039cfb64efb0885eed909a36bcf''
		]
	},
	HiExampleCommit {
		#id : ''ecf29a507e626365d52d4f574d88284f55586a1e'',
		#comment : ''Fixes #701\nExtracting Merge logic into a reusable command.\nModifying the Merge preview to only show new elements.\nAdding tests'',
		#parentIds : [
			''86f6770b2960957496984998c0731a9e0f8e7671''
		]
	},
	HiExampleCommit {
		#id : ''55510b31db672e1e7ca8f21b872c98f10dffdf41'',
		#comment : ''Add a comment in the README about obsolete classes\n\nFixes https://github.com/pharo-vcs/iceberg/issues/697'',
		#parentIds : [
			''78a671f1ef1231acef4d39097339350dc2a090c7''
		]
	},
	HiExampleCommit {
		#id : ''d49ca0e3b75b0039cfb64efb0885eed909a36bcf'',
		#comment : ''Merge pull request #713 from jecisc/cf_IcePharoPluginShouldOnlyWorkInPharo7AndSuperior\n\nIcePharoPlugin should not care of Pharo 6 since it\''ll just bug.'',
		#parentIds : [
			''2a36d470a4bfb8008cd83368c055f3edb0b8f52f'',
			''34ee9f6d311b9d996ac40afdc0ecfdb112c5270b''
		]
	},
	HiExampleCommit {
		#id : ''78a671f1ef1231acef4d39097339350dc2a090c7'',
		#comment : ''Merge pull request #699 from jecisc/cf_updateReadMeToRemoveAllNeededPackages\n\nAdd more packages to remove on Iceberg update.'',
		#parentIds : [
			''a2ba9f7d29e3dbb7fa79571d10ae5866f6fda3e2'',
			''0a801c5690f079357284361e631fe4d026d67329''
		]
	},
	HiExampleCommit {
		#id : ''2a36d470a4bfb8008cd83368c055f3edb0b8f52f'',
		#comment : ''Merge pull request #700 from jecisc/cf_correctAddPackagesFilter\n\ncf_correctAddPackagesFilter'',
		#parentIds : [
			''1e604aba87f85ea803dbad74e66a1b6ab9c2185c'',
			''0801f53c3412defdda7e22ea9e146cc13ff7ae7d''
		]
	},
	HiExampleCommit {
		#id : ''34ee9f6d311b9d996ac40afdc0ecfdb112c5270b'',
		#comment : ''IcePharoPlugin should not care of Pharo 6 since it\''ll just bug.\n\nFixes https://github.com/pharo-vcs/iceberg/issues/619'',
		#parentIds : [
			''1e604aba87f85ea803dbad74e66a1b6ab9c2185c''
		]
	},
	HiExampleCommit {
		#id : ''ea67cc841d6e08aed194800bcb08100d93cb9ff9'',
		#comment : ''Merge 1e604aba87f85ea803dbad74e66a1b6ab9c2185c'',
		#parentIds : [
			''a963f4f4de7334a43687dad35b806120ac9498a3'',
			''1e604aba87f85ea803dbad74e66a1b6ab9c2185c''
		]
	},
	HiExampleCommit {
		#id : ''a963f4f4de7334a43687dad35b806120ac9498a3'',
		#comment : ''refactored'',
		#parentIds : [
			''9cc87e6da0d35802b58b61ad04d4597abf218b1a''
		]
	},
	HiExampleCommit {
		#id : ''86f6770b2960957496984998c0731a9e0f8e7671'',
		#comment : ''Fixing the other tests that guille hide (in the past)'',
		#parentIds : [
			''1e604aba87f85ea803dbad74e66a1b6ab9c2185c''
		]
	},
	HiExampleCommit {
		#id : ''1e604aba87f85ea803dbad74e66a1b6ab9c2185c'',
		#comment : ''Fixing the test that Guille made me breaks '',
		#parentIds : [
			''8fb6c13c9254d945980b4a6cbaee835ecead5742''
		]
	},
	HiExampleCommit {
		#id : ''8fb6c13c9254d945980b4a6cbaee835ecead5742'',
		#comment : ''Merge e2802fb5e67724aee27579d7ffd7b5ebc3be1ec3'',
		#parentIds : [
			''bd3a542205d4c5b371571a9b3755a360d5fd4b30'',
			''e2802fb5e67724aee27579d7ffd7b5ebc3be1ec3''
		]
	},
	HiExampleCommit {
		#id : ''bd3a542205d4c5b371571a9b3755a360d5fd4b30'',
		#comment : ''Adding test to validate issue #710'',
		#parentIds : [
			''9af956316cacc05ef812a3fb2c2da7a71bdec37e''
		]
	},
	HiExampleCommit {
		#id : ''9af956316cacc05ef812a3fb2c2da7a71bdec37e'',
		#comment : ''Fixing the fast forward merge.'',
		#parentIds : [
			''9681d312561334015a54dc077154902b0313c978''
		]
	},
	HiExampleCommit {
		#id : ''e2802fb5e67724aee27579d7ffd7b5ebc3be1ec3'',
		#comment : ''Fix #705\n- Extract git clone into an independent object\n- Extended IceRepositoryCreator to optionally receive a repository object to edit'',
		#parentIds : [
			''712c7c8e0d656084a211cb0ae1c1c0ceb43ee166''
		]
	},
	HiExampleCommit {
		#id : ''712c7c8e0d656084a211cb0ae1c1c0ceb43ee166'',
		#comment : ''Add missing repository test and fixture'',
		#parentIds : [
			''9681d312561334015a54dc077154902b0313c978''
		]
	},
	HiExampleCommit {
		#id : ''9681d312561334015a54dc077154902b0313c978'',
		#comment : ''Merge 042319340d1cc4a0e8c8e3a3d86553d8c3642125'',
		#parentIds : [
			''da9eacfe6af3c9e44a9d3f2516491d29b6bab02a'',
			''042319340d1cc4a0e8c8e3a3d86553d8c3642125''
		]
	},
	HiExampleCommit {
		#id : ''da9eacfe6af3c9e44a9d3f2516491d29b6bab02a'',
		#comment : ''Fix #694\nAvoid diffing if reference commit is = to adopted commit\n'',
		#parentIds : [
			''680cc5a77bf892d58bc96f8379ac469730f31747''
		]
	},
	HiExampleCommit {
		#id : ''0801f53c3412defdda7e22ea9e146cc13ff7ae7d'',
		#comment : ''Add test for IceTipPackageFilter>>matches:'',
		#parentIds : [
			''2b3a9486bf393f59771c9cc912bdaf17a59b6d4a''
		]
	},
	HiExampleCommit {
		#id : ''2b3a9486bf393f59771c9cc912bdaf17a59b6d4a'',
		#comment : ''When adding a package, if we filter the list, use #asLowercase on both package name and pattern from the user.'',
		#parentIds : [
			''042319340d1cc4a0e8c8e3a3d86553d8c3642125''
		]
	},
	HiExampleCommit {
		#id : ''0a801c5690f079357284361e631fe4d026d67329'',
		#comment : ''Add more packages to remove on Iceberg update.\n\n'',
		#parentIds : [
			''a2ba9f7d29e3dbb7fa79571d10ae5866f6fda3e2''
		]
	},
	HiExampleCommit {
		#id : ''a2ba9f7d29e3dbb7fa79571d10ae5866f6fda3e2'',
		#comment : ''Merge branch \''dev-0.7\''\n'',
		#parentIds : [
			''22fa2112dbf564d7dccf975bdcdab9660674f658'',
			''042319340d1cc4a0e8c8e3a3d86553d8c3642125''
		]
	},
	HiExampleCommit {
		#id : ''042319340d1cc4a0e8c8e3a3d86553d8c3642125'',
		#comment : ''Fixing the adopt of a unknownCommit\nFixing creation of unknown commit'',
		#parentIds : [
			''825894e525f2f23572e532e839deedd328f956b8''
		]
	},
	HiExampleCommit {
		#id : ''22fa2112dbf564d7dccf975bdcdab9660674f658'',
		#comment : ''Merge pull request #696 from pharo-vcs/dev-0.7\n\nFixing problem in the integration with iceberg.'',
		#parentIds : [
			''2e64f9c4d16de1987752a120bb4449a843eefa1b'',
			''825894e525f2f23572e532e839deedd328f956b8''
		]
	},
	HiExampleCommit {
		#id : ''825894e525f2f23572e532e839deedd328f956b8'',
		#comment : ''Fixing problem in the integration with iceberg.'',
		#parentIds : [
			''34776236f8ddb2237efe640cff418f5f2d5df834''
		]
	},
	HiExampleCommit {
		#id : ''2e64f9c4d16de1987752a120bb4449a843eefa1b'',
		#comment : ''Merge pull request #693 from pharo-vcs/dev-0.7\n\nFixing a problem when the Announced package is nil'',
		#parentIds : [
			''89a5cfa988bf0f46ccc606561b7e633619ac68f4'',
			''34776236f8ddb2237efe640cff418f5f2d5df834''
		]
	},
	HiExampleCommit {
		#id : ''34776236f8ddb2237efe640cff418f5f2d5df834'',
		#comment : ''Merge pull request #692 from tesonep/issue/691\n\nissue/691'',
		#parentIds : [
			''680cc5a77bf892d58bc96f8379ac469730f31747'',
			''7585a3072304ecd85fc5815d37754acf75e4450f''
		]
	},
	HiExampleCommit {
		#id : ''7585a3072304ecd85fc5815d37754acf75e4450f'',
		#comment : ''The affected package in the notification can be nil. \nIf the package does not exists the package is nil.'',
		#parentIds : [
			''680cc5a77bf892d58bc96f8379ac469730f31747''
		]
	},
	HiExampleCommit {
		#id : ''89a5cfa988bf0f46ccc606561b7e633619ac68f4'',
		#comment : ''Merge pull request #687 from pharo-vcs/dev-0.7\n\nPatch diff calculation on unknown commit'',
		#parentIds : [
			''5303ca91df890aa6e1211a9507edd0629d92a539'',
			''680cc5a77bf892d58bc96f8379ac469730f31747''
		]
	},
	HiExampleCommit {
		#id : ''680cc5a77bf892d58bc96f8379ac469730f31747'',
		#comment : ''Patch diff calculation on unknown commit'',
		#parentIds : [
			''a63ec81d94a5d98a22edad3be24d330f49d41ad4''
		]
	},
	HiExampleCommit {
		#id : ''5303ca91df890aa6e1211a9507edd0629d92a539'',
		#comment : ''Merge pull request #685 from pharo-vcs/dev-0.7\n\nAdd case for location in nil'',
		#parentIds : [
			''d25724a43f12f05882e0f9900bb9f153bcfa1d36'',
			''a63ec81d94a5d98a22edad3be24d330f49d41ad4''
		]
	},
	HiExampleCommit {
		#id : ''a63ec81d94a5d98a22edad3be24d330f49d41ad4'',
		#comment : ''Add case for location in nil'',
		#parentIds : [
			''9cc87e6da0d35802b58b61ad04d4597abf218b1a''
		]
	},
	HiExampleCommit {
		#id : ''d25724a43f12f05882e0f9900bb9f153bcfa1d36'',
		#comment : ''Merge pull request #684 from pharo-vcs/dev-0.7\n\nrelease 0.7.3'',
		#parentIds : [
			''79f17f634ffac14b4a0c31c5bc0aca641ca8449e'',
			''9cc87e6da0d35802b58b61ad04d4597abf218b1a''
		]
	},
	HiExampleCommit {
		#id : ''9cc87e6da0d35802b58b61ad04d4597abf218b1a'',
		#comment : ''Recover changes in IcePharoFogbugzPanel'',
		#parentIds : [
			''88f02d58aa09d7051e222c9f86adfdaabab3341a''
		]
	},
	HiExampleCommit {
		#id : ''88f02d58aa09d7051e222c9f86adfdaabab3341a'',
		#comment : ''Merge 690403afc71abbee3c663e70f09692836a016e19'',
		#parentIds : [
			''410a256924669c1de259ea1cdafb2e0c050eb968'',
			''690403afc71abbee3c663e70f09692836a016e19''
		]
	},
	HiExampleCommit {
		#id : ''410a256924669c1de259ea1cdafb2e0c050eb968'',
		#comment : ''Accellerate snapshot by avoiding progress bar'',
		#parentIds : [
			''1c9a78b0812df8a1a8773fa3caeb2354a4bcc78e''
		]
	},
	HiExampleCommit {
		#id : ''690403afc71abbee3c663e70f09692836a016e19'',
		#comment : ''Merge 4aa016c1568e41f527f6a7608ff76542c601f820'',
		#parentIds : [
			''a814ff9eed58b4d24f125e8ca877a6f5ca27a4c8'',
			''4aa016c1568e41f527f6a7608ff76542c601f820''
		]
	},
	HiExampleCommit {
		#id : ''a814ff9eed58b4d24f125e8ca877a6f5ca27a4c8'',
		#comment : ''updated to v1.3.1'',
		#parentIds : [
			''1f380d04e446dfbf1bee47da21af2ad6d245a66e''
		]
	},
	HiExampleCommit {
		#id : ''4aa016c1568e41f527f6a7608ff76542c601f820'',
		#comment : ''Merge pull request #683 from tesonep/issue/666\n\nFixes #666'',
		#parentIds : [
			''1f380d04e446dfbf1bee47da21af2ad6d245a66e'',
			''e1a8ebe66f03377a2d8ef56bddf008652e293506''
		]
	},
	HiExampleCommit {
		#id : ''1f380d04e446dfbf1bee47da21af2ad6d245a66e'',
		#comment : ''Merge branch \''dev-0.7\'' of github.com:pharo-vcs/iceberg into dev-0.7\n\n* \''dev-0.7\'' of github.com:pharo-vcs/iceberg:\n  Updating smalltalk ci pre-script\n'',
		#parentIds : [
			''597fc5a17a73c17a72b2dc86898042e5e3a6b7be'',
			''28d290308a9e5f0f1ce4bbe7dce1842644de0cfa''
		]
	},
	HiExampleCommit {
		#id : ''597fc5a17a73c17a72b2dc86898042e5e3a6b7be'',
		#comment : ''updated update script for 6.1 to 0.7.? (like that they take always last version)\n'',
		#parentIds : [
			''9b7fbb85904fee1b4ed28f564a7317690db66f79''
		]
	},
	HiExampleCommit {
		#id : ''e1a8ebe66f03377a2d8ef56bddf008652e293506'',
		#comment : ''Fixes #666\nThere was a problem when we were passing a byteArray instead of an String in the FFI call.'',
		#parentIds : [
			''2d29207c261b8c7f309254c78ca52487fcc148f7''
		]
	},
	HiExampleCommit {
		#id : ''28d290308a9e5f0f1ce4bbe7dce1842644de0cfa'',
		#comment : ''Updating smalltalk ci pre-script'',
		#parentIds : [
			''3cecc05ed5ab6707ad822f6435038e8fc5539fc8''
		]
	},
	HiExampleCommit {
		#id : ''9b7fbb85904fee1b4ed28f564a7317690db66f79'',
		#comment : ''Merge branch \''master\'' of github.com:pharo-vcs/iceberg into dev-0.7\n'',
		#parentIds : [
			''3cecc05ed5ab6707ad822f6435038e8fc5539fc8'',
			''79f17f634ffac14b4a0c31c5bc0aca641ca8449e''
		]
	},
	HiExampleCommit {
		#id : ''3cecc05ed5ab6707ad822f6435038e8fc5539fc8'',
		#comment : ''added an utility method (I don\''t know where else to put it :P)'',
		#parentIds : [
			''1c9a78b0812df8a1a8773fa3caeb2354a4bcc78e''
		]
	},
	HiExampleCommit {
		#id : ''1c9a78b0812df8a1a8773fa3caeb2354a4bcc78e'',
		#comment : ''Fix #681$'',
		#parentIds : [
			''2d29207c261b8c7f309254c78ca52487fcc148f7''
		]
	},
	HiExampleCommit {
		#id : ''2d29207c261b8c7f309254c78ca52487fcc148f7'',
		#comment : ''Remove duplicated tests'',
		#parentIds : [
			''19d8b6ea305654835a623098080db115b6eeff1c''
		]
	},
	HiExampleCommit {
		#id : ''19d8b6ea305654835a623098080db115b6eeff1c'',
		#comment : ''Finish merge due to rename'',
		#parentIds : [
			''e25a99adecfaedba40e7afb3a134ae38ddfeb963''
		]
	},
	HiExampleCommit {
		#id : ''e25a99adecfaedba40e7afb3a134ae38ddfeb963'',
		#comment : ''Merge ebd486937368902beb7459f8ecd4d51120ebce9f'',
		#parentIds : [
			''0c76de04cc059ca80d71432ab0069ba343cb72db'',
			''ebd486937368902beb7459f8ecd4d51120ebce9f''
		]
	},
	HiExampleCommit {
		#id : ''ebd486937368902beb7459f8ecd4d51120ebce9f'',
		#comment : ''Make all tests pass'',
		#parentIds : [
			''f0072a870dceda0aa59d0d7adc828777e587397b''
		]
	},
	HiExampleCommit {
		#id : ''116f64cb5eef67874cb03195140549ca0b0b2911'',
		#comment : ''Update .travis.yml\n\nTest 6.1 and 64 bits'',
		#parentIds : [
			''0c76de04cc059ca80d71432ab0069ba343cb72db''
		]
	},
	HiExampleCommit {
		#id : ''f0072a870dceda0aa59d0d7adc828777e587397b'',
		#comment : ''- removing files\n- refactor visitors\n- add tests '',
		#parentIds : [
			''69ab45096f87c07dcf4ba88ab2dab7fe11eea841''
		]
	},
	HiExampleCommit {
		#id : ''0c76de04cc059ca80d71432ab0069ba343cb72db'',
		#comment : ''Fix #680$'',
		#parentIds : [
			''c62aee8169d0623bffbf46555b1991c88ea154ac''
		]
	},
	HiExampleCommit {
		#id : ''c62aee8169d0623bffbf46555b1991c88ea154ac'',
		#comment : ''Make memory tests green'',
		#parentIds : [
			''66e81c3b58826362b41136aad2a903827e24cfde''
		]
	},
	HiExampleCommit {
		#id : ''66e81c3b58826362b41136aad2a903827e24cfde'',
		#comment : ''Fix #679$'',
		#parentIds : [
			''989696eae5a1804196466951245a5ad16f6c05c4''
		]
	},
	HiExampleCommit {
		#id : ''989696eae5a1804196466951245a5ad16f6c05c4'',
		#comment : ''- Added unknown commit tests\n- fetch calls post-fetch action to rebind unknown commits'',
		#parentIds : [
			''69ab45096f87c07dcf4ba88ab2dab7fe11eea841''
		]
	},
	HiExampleCommit {
		#id : ''69ab45096f87c07dcf4ba88ab2dab7fe11eea841'',
		#comment : ''Merge ce190a1d6f55274a2ada027d25fddc02010ea871'',
		#parentIds : [
			''2376c1b544a0da23538293913f52fea177c9879c'',
			''ce190a1d6f55274a2ada027d25fddc02010ea871''
		]
	},
	HiExampleCommit {
		#id : ''ce190a1d6f55274a2ada027d25fddc02010ea871'',
		#comment : ''Tests for case #583\n - test default properties\n - test properties file is added to the index'',
		#parentIds : [
			''1abbdbe0975ed0be2b280bdf829ad21f1e0dd4e3''
		]
	},
	HiExampleCommit {
		#id : ''1abbdbe0975ed0be2b280bdf829ad21f1e0dd4e3'',
		#comment : ''Added properties tests'',
		#parentIds : [
			''d703233d795ccc16dc24604f9cadad52407dbcd0''
		]
	},
	HiExampleCommit {
		#id : ''d703233d795ccc16dc24604f9cadad52407dbcd0'',
		#comment : ''Getting properties from commit to grab the correct format'',
		#parentIds : [
			''608815d5e20262fb18f57337880199923ef3af62''
		]
	},
	HiExampleCommit {
		#id : ''608815d5e20262fb18f57337880199923ef3af62'',
		#comment : ''Fix metacello integration tests'',
		#parentIds : [
			''96d89440e479b954ce61aaf248c7a17710dda4a9''
		]
	},
	HiExampleCommit {
		#id : ''96d89440e479b954ce61aaf248c7a17710dda4a9'',
		#comment : ''Remove unused methods'',
		#parentIds : [
			''7d5121bc056664266f5dbb2e134d32dddcd8fd80''
		]
	},
	HiExampleCommit {
		#id : ''2376c1b544a0da23538293913f52fea177c9879c'',
		#comment : ''removed again because is not working properly'',
		#parentIds : [
			''7d5121bc056664266f5dbb2e134d32dddcd8fd80''
		]
	},
	HiExampleCommit {
		#id : ''7d5121bc056664266f5dbb2e134d32dddcd8fd80'',
		#comment : ''Recover #657 from bad merge'',
		#parentIds : [
			''dc3812933e75224d873a662ada2374153cc1eedc''
		]
	},
	HiExampleCommit {
		#id : ''dc3812933e75224d873a662ada2374153cc1eedc'',
		#comment : ''Added Tests for dettached working copy.\nPossibly fix #609'',
		#parentIds : [
			''ad045fe291111dcd5d11765c913327e2af30b604''
		]
	},
	HiExampleCommit {
		#id : ''ad045fe291111dcd5d11765c913327e2af30b604'',
		#comment : ''Merge pull request #663 from tesonep/only-fetching-if-needed-in-pharo\n\nThe IcePharoPlugin should only fetch if the commit is not in the repoâ\u0080¦'',
		#parentIds : [
			''e415d27551b045d3763bfa4e7cea3c3779a16778'',
			''47f3808381580a5418b5b474e894a097608b05c8''
		]
	},
	HiExampleCommit {
		#id : ''e415d27551b045d3763bfa4e7cea3c3779a16778'',
		#comment : ''Recover badge changes'',
		#parentIds : [
			''02f8cb56dd5bc52b01cfd4990dbe818211a936e8''
		]
	},
	HiExampleCommit {
		#id : ''47f3808381580a5418b5b474e894a097608b05c8'',
		#comment : ''The IcePharoPlugin should only fetch if the commit is not in the repository'',
		#parentIds : [
			''39a9972ef8bc991f8c28ab5069c23b0e89c967dc''
		]
	},
	HiExampleCommit {
		#id : ''02f8cb56dd5bc52b01cfd4990dbe818211a936e8'',
		#comment : ''Merge 39a9972ef8bc991f8c28ab5069c23b0e89c967dc'',
		#parentIds : [
			''1483fc55d7d9b1acfa57dc295ad745401cc473b9'',
			''39a9972ef8bc991f8c28ab5069c23b0e89c967dc''
		]
	},
	HiExampleCommit {
		#id : ''1483fc55d7d9b1acfa57dc295ad745401cc473b9'',
		#comment : ''Remove debugging code'',
		#parentIds : [
			''c149a30e73b2adc3086b313c1081bc8ac3f0d99f''
		]
	},
	HiExampleCommit {
		#id : ''39a9972ef8bc991f8c28ab5069c23b0e89c967dc'',
		#comment : ''Merge pull request #661 from guillep/issue/639\n\nFix issue #639'',
		#parentIds : [
			''7ff9983dd8eadcff16609b2a1404ee8f0f62df22'',
			''75430193d4e0d70c1ad79d393deb24f97a25ae57''
		]
	},
	HiExampleCommit {
		#id : ''7ff9983dd8eadcff16609b2a1404ee8f0f62df22'',
		#comment : ''better handling of refresh'',
		#parentIds : [
			''7882b50a57ed21eb93c73586f517e5f01df9f5a9''
		]
	},
	HiExampleCommit {
		#id : ''7882b50a57ed21eb93c73586f517e5f01df9f5a9'',
		#comment : ''Merge c149a30e73b2adc3086b313c1081bc8ac3f0d99f'',
		#parentIds : [
			''ea241687a904ea6039f9b0105381f9c1ee3d808b'',
			''c149a30e73b2adc3086b313c1081bc8ac3f0d99f''
		]
	},
	HiExampleCommit {
		#id : ''ea241687a904ea6039f9b0105381f9c1ee3d808b'',
		#comment : ''better handling of refresh'',
		#parentIds : [
			''d649e7b7e4565d6d887c487e803fbf6e40afd21d''
		]
	},
	HiExampleCommit {
		#id : ''c149a30e73b2adc3086b313c1081bc8ac3f0d99f'',
		#comment : ''Merge pull request #660 from guillep/issue/636\n\nAdd a star badge in the push button in case of a new branch'',
		#parentIds : [
			''0a6374fc3b36f000e5a8f9099d2ff8b58f547e37'',
			''63932b4ae337aa01bf202a73c3323b6fb647880b''
		]
	},
	HiExampleCommit {
		#id : ''75430193d4e0d70c1ad79d393deb24f97a25ae57'',
		#comment : ''Better error reporting on github error'',
		#parentIds : [
			''0a6374fc3b36f000e5a8f9099d2ff8b58f547e37''
		]
	},
	HiExampleCommit {
		#id : ''d649e7b7e4565d6d887c487e803fbf6e40afd21d'',
		#comment : ''remove bad refresh (it was incorrectly reseting models)'',
		#parentIds : [
			''f1b9e76e21a4d18e8fa703de133dfe07a3ba16e4''
		]
	},
	HiExampleCommit {
		#id : ''0a6374fc3b36f000e5a8f9099d2ff8b58f547e37'',
		#comment : ''Fix issue #657\nAdd \''e.g.,\'' to all ghost texts\'''',
		#parentIds : [
			''5d96d9d525ab35c49787477bdc666011a853bd47''
		]
	},
	HiExampleCommit {
		#id : ''f1b9e76e21a4d18e8fa703de133dfe07a3ba16e4'',
		#comment : ''fix pull request fetch'',
		#parentIds : [
			''5d96d9d525ab35c49787477bdc666011a853bd47''
		]
	},
	HiExampleCommit {
		#id : ''63932b4ae337aa01bf202a73c3323b6fb647880b'',
		#comment : ''Avoid unecessary string conversion to avoid "0" badges'',
		#parentIds : [
			''2bfc3b1bce5e2b7cb86d435c90f9d1c85e03f70d''
		]
	},
	HiExampleCommit {
		#id : ''2bfc3b1bce5e2b7cb86d435c90f9d1c85e03f70d'',
		#comment : ''Better badge font and putting a * in case of new branch'',
		#parentIds : [
			''46178d8f6631f711dacf0584fd8698d0a33ee032''
		]
	},
	HiExampleCommit {
		#id : ''5d96d9d525ab35c49787477bdc666011a853bd47'',
		#comment : ''Merge 46178d8f6631f711dacf0584fd8698d0a33ee032'',
		#parentIds : [
			''f1fe4f9696e0a00b3c2c76cd2ed733904a205273'',
			''46178d8f6631f711dacf0584fd8698d0a33ee032''
		]
	},
	HiExampleCommit {
		#id : ''f1fe4f9696e0a00b3c2c76cd2ed733904a205273'',
		#comment : ''restore retry with HTTPS fallback when creating repositories after a metacello adapter (to allow install on non-authenticathed machines).\n\nfixes #643'',
		#parentIds : [
			''5587bfd93728cd5076bc244ac0d81222072c0872''
		]
	},
	HiExampleCommit {
		#id : ''46178d8f6631f711dacf0584fd8698d0a33ee032'',
		#comment : ''Merge c84694889f64563e3fda7b95aa9933fa7b5769f3'',
		#parentIds : [
			''ec400d319c0c5d8afe0532e138e5f82b4b65beea'',
			''c84694889f64563e3fda7b95aa9933fa7b5769f3''
		]
	},
	HiExampleCommit {
		#id : ''ec400d319c0c5d8afe0532e138e5f82b4b65beea'',
		#comment : ''Fixing the creation of branch from issue when done in the context menu.'',
		#parentIds : [
			''a0c189edf66be5b1fc3a572723a3b43290bd734c''
		]
	},
	HiExampleCommit {
		#id : ''c84694889f64563e3fda7b95aa9933fa7b5769f3'',
		#comment : ''Merge pull request #652 from guillep/issue/614\n\nFixing Issue #614'',
		#parentIds : [
			''c825686252949de18ae5c3767374838801e38db0'',
			''54f9210b02915843678c36cd1ee394e1e82d91a4''
		]
	},
	HiExampleCommit {
		#id : ''54f9210b02915843678c36cd1ee394e1e82d91a4'',
		#comment : ''Make tests work in the absence of the username property'',
		#parentIds : [
			''99d375ef28bbe92de9d9d9e7b468ad453fc41c4c''
		]
	},
	HiExampleCommit {
		#id : ''c825686252949de18ae5c3767374838801e38db0'',
		#comment : ''Merge pull request #659 from tesonep/improving-checkout-progress-bar\n\nImproving the progress bar.'',
		#parentIds : [
			''9785fc4437a80fa49f7b33c43a498110af2e8f26'',
			''33c9e4a301fc1f6dee814ee9c83bc923efe4ffdd''
		]
	},
	HiExampleCommit {
		#id : ''5587bfd93728cd5076bc244ac0d81222072c0872'',
		#comment : ''add missing error handlers'',
		#parentIds : [
			''9785fc4437a80fa49f7b33c43a498110af2e8f26''
		]
	},
	HiExampleCommit {
		#id : ''33c9e4a301fc1f6dee814ee9c83bc923efe4ffdd'',
		#comment : ''Adding missing implementation in IceMemoryChange$'',
		#parentIds : [
			''02cd110c31e7333df9e270fd02bb38643db8c321''
		]
	},
	HiExampleCommit {
		#id : ''99d375ef28bbe92de9d9d9e7b468ad453fc41c4c'',
		#comment : ''Using libgit 1.3.0'',
		#parentIds : [
			''8f8d074db22c0bea7c362e2ab0aeb855c07c1b88''
		]
	},
	HiExampleCommit {
		#id : ''9785fc4437a80fa49f7b33c43a498110af2e8f26'',
		#comment : ''Merge pull request #654 from tesonep/pushing-a-new-branch-has-empty-commit-list\n\nPushing a new branch has empty commit list.'',
		#parentIds : [
			''f562160b915a57232b3a9500dc96cccc9aec0b40'',
			''4a007a5ef3110de636c364bc85ae436916cfd72c''
		]
	},
	HiExampleCommit {
		#id : ''8f8d074db22c0bea7c362e2ab0aeb855c07c1b88'',
		#comment : ''Added tests'',
		#parentIds : [
			''ca9d0299b4b44b06607ff6e06f43e39c5992e83b''
		]
	},
	HiExampleCommit {
		#id : ''02cd110c31e7333df9e270fd02bb38643db8c321'',
		#comment : ''Moving to the correct package'',
		#parentIds : [
			''6d18a59b8778b6a41dd9d9df736a4e3bfb4dffd8''
		]
	},
	HiExampleCommit {
		#id : ''f562160b915a57232b3a9500dc96cccc9aec0b40'',
		#comment : ''refactored remote selection to allow also add missing remotes\n\nFixes #624'',
		#parentIds : [
			''22c217c85857fc89dfb8b9d028b714ac566cca20''
		]
	},
	HiExampleCommit {
		#id : ''ca9d0299b4b44b06607ff6e06f43e39c5992e83b'',
		#comment : ''Making dialog and exception work together'',
		#parentIds : [
			''72fc7f8b737fab0cacd4110c42cb2c939b3ea492''
		]
	},
	HiExampleCommit {
		#id : ''72fc7f8b737fab0cacd4110c42cb2c939b3ea492'',
		#comment : ''Adding handler in visitor'',
		#parentIds : [
			''a95830b085170c3f5e3d7ff54ff324f3e71a28af''
		]
	},
	HiExampleCommit {
		#id : ''a95830b085170c3f5e3d7ff54ff324f3e71a28af'',
		#comment : ''Introduce user.name user.mail dialog as tip dialog'',
		#parentIds : [
			''a0c189edf66be5b1fc3a572723a3b43290bd734c''
		]
	},
	HiExampleCommit {
		#id : ''6d18a59b8778b6a41dd9d9df736a4e3bfb4dffd8'',
		#comment : ''Improving the progress bar.\n\nFixes #656\nFixes #658'',
		#parentIds : [
			''a0c189edf66be5b1fc3a572723a3b43290bd734c''
		]
	},
	HiExampleCommit {
		#id : ''22c217c85857fc89dfb8b9d028b714ac566cca20'',
		#comment : ''renamed and moved to IceTip package'',
		#parentIds : [
			''a0c189edf66be5b1fc3a572723a3b43290bd734c''
		]
	},
	HiExampleCommit {
		#id : ''a0c189edf66be5b1fc3a572723a3b43290bd734c'',
		#comment : ''add ghost text'',
		#parentIds : [
			''c3b4fcc47a90ebadf6c1cf365952b71a38fd865c''
		]
	},
	HiExampleCommit {
		#id : ''c3b4fcc47a90ebadf6c1cf365952b71a38fd865c'',
		#comment : ''fix pharo issue dialog'',
		#parentIds : [
			''765fa81e5b0bce19cab70d3edee84bf51e144eac''
		]
	},
	HiExampleCommit {
		#id : ''4a007a5ef3110de636c364bc85ae436916cfd72c'',
		#comment : ''When pushing a new branch it should show all the commits. As this can be a lot, we are showing 20.'',
		#parentIds : [
			''5d8df258f48bd565ad47a01cbc09f6fee6fda0d4''
		]
	},
	HiExampleCommit {
		#id : ''765fa81e5b0bce19cab70d3edee84bf51e144eac'',
		#comment : ''Open Pharo issue creation dialog$'',
		#parentIds : [
			''5d8df258f48bd565ad47a01cbc09f6fee6fda0d4''
		]
	},
	HiExampleCommit {
		#id : ''5d8df258f48bd565ad47a01cbc09f6fee6fda0d4'',
		#comment : ''Fixes #647'',
		#parentIds : [
			''f54734c3a6f3ad1fd786bd85a9f15e0e4c0594a1''
		]
	},
	HiExampleCommit {
		#id : ''f54734c3a6f3ad1fd786bd85a9f15e0e4c0594a1'',
		#comment : ''Fixes #644 (not for real)'',
		#parentIds : [
			''72f7f965bb00e995ca62def522ab8bca52fafda1''
		]
	},
	HiExampleCommit {
		#id : ''72f7f965bb00e995ca62def522ab8bca52fafda1'',
		#comment : ''recategorisation\n\nFixes #644'',
		#parentIds : [
			''b20bf979f72900d0b57ff279dccc77c8be31d247''
		]
	},
	HiExampleCommit {
		#id : ''b20bf979f72900d0b57ff279dccc77c8be31d247'',
		#comment : ''fix locate when location is nil'',
		#parentIds : [
			''5fab51243dde75c3de8c356ef19c4d188ad7bd0b''
		]
	},
	HiExampleCommit {
		#id : ''5fab51243dde75c3de8c356ef19c4d188ad7bd0b'',
		#comment : ''fetching just "pharo-project" when adding'',
		#parentIds : [
			''1f5025aa2e2a393601899084fe5d27722e3b09d1''
		]
	},
	HiExampleCommit {
		#id : ''1f5025aa2e2a393601899084fe5d27722e3b09d1'',
		#comment : ''set workingCopy repository when assigning a new working copy'',
		#parentIds : [
			''8629796589dda71f03164f6e1c317ec44ab21d1f''
		]
	},
	HiExampleCommit {
		#id : ''8629796589dda71f03164f6e1c317ec44ab21d1f'',
		#comment : ''clean'',
		#parentIds : [
			''b2bcba0c55eeb62e02ee199632dd7758b1c099b7''
		]
	},
	HiExampleCommit {
		#id : ''b2bcba0c55eeb62e02ee199632dd7758b1c099b7'',
		#comment : ''set of commit moved to *before* verify package. Otherwise it will assign the unknown commit when it shouldn\''t'',
		#parentIds : [
			''8c4b8f34dba62135f2069af178c2d560bdecb18e''
		]
	},
	HiExampleCommit {
		#id : ''79f17f634ffac14b4a0c31c5bc0aca641ca8449e'',
		#comment : ''Merge pull request #642 from jecisc/cf_IgnoreWarningsWhileLoadingInPharo6\n\nIgnore warnings while loading Iceberg 0.7 in Pharo 6.1'',
		#parentIds : [
			''f10f3c38ab4454b8be4884ba7ae5801a22b53336'',
			''2546f3d4efcd5b8ac3c28b39fe89e57e20a6869b''
		]
	},
	HiExampleCommit {
		#id : ''2546f3d4efcd5b8ac3c28b39fe89e57e20a6869b'',
		#comment : ''Ignore warnings while loading Iceberg 0.7 in Pharo 6.1\n'',
		#parentIds : [
			''f10f3c38ab4454b8be4884ba7ae5801a22b53336''
		]
	},
	HiExampleCommit {
		#id : ''8c4b8f34dba62135f2069af178c2d560bdecb18e'',
		#comment : ''Merge pull request #641 from guillep/enhancement/upgrade-libgit\n\nUpgrade libgit to version 1.2.1.'',
		#parentIds : [
			''23f8e73ff9ed3af7d5e39a83ce75e6de3673827c'',
			''36e39076d3c5fe515c10c09b93d98d5e3034fd1b''
		]
	},
	HiExampleCommit {
		#id : ''23f8e73ff9ed3af7d5e39a83ce75e6de3673827c'',
		#comment : ''Merge pull request #640 from guillep/cleanup/remove-unused-tests\n\nRemove unused abstract test classes'',
		#parentIds : [
			''a2324f785e565755e8dda6354a02a83a0504c84f'',
			''125864a560d2a7c59241b627b9ddca665832e4fc''
		]
	},
	HiExampleCommit {
		#id : ''36e39076d3c5fe515c10c09b93d98d5e3034fd1b'',
		#comment : ''Upgrade libgit to version 1.2.1.\nSimplified shutdown'',
		#parentIds : [
			''a2324f785e565755e8dda6354a02a83a0504c84f''
		]
	},
	HiExampleCommit {
		#id : ''125864a560d2a7c59241b627b9ddca665832e4fc'',
		#comment : ''Restore github user.name and user.mail'',
		#parentIds : [
			''b1a0a11f859ee0053139f568bd62e2cdfdd15d2b''
		]
	},
	HiExampleCommit {
		#id : ''b1a0a11f859ee0053139f568bd62e2cdfdd15d2b'',
		#comment : ''Remove unused abstract test classes'',
		#parentIds : [
			''a2324f785e565755e8dda6354a02a83a0504c84f''
		]
	},
	HiExampleCommit {
		#id : ''a2324f785e565755e8dda6354a02a83a0504c84f'',
		#comment : ''Merge pull request #638 from guillep/refactor/separate-fixtures\n\nRefactor tests to split fixtures'',
		#parentIds : [
			''a3c5c83f180b72bf7aff9adaf96543a436d60b6b'',
			''b90af59743cd02c2b8b46a75c229aa11fc86aa60''
		]
	},
	HiExampleCommit {
		#id : ''b90af59743cd02c2b8b46a75c229aa11fc86aa60'',
		#comment : ''Fixed merge with dev-0.7$'',
		#parentIds : [
			''de618bb650fdf1155c426f7e255b7b1e92de69af''
		]
	},
	HiExampleCommit {
		#id : ''de618bb650fdf1155c426f7e255b7b1e92de69af'',
		#comment : ''Merge a3c5c83f180b72bf7aff9adaf96543a436d60b6b'',
		#parentIds : [
			''55104e55effce7ddbb96cbbac1b116258ffb6bcd'',
			''a3c5c83f180b72bf7aff9adaf96543a436d60b6b''
		]
	},
	HiExampleCommit {
		#id : ''55104e55effce7ddbb96cbbac1b116258ffb6bcd'',
		#comment : ''Refactor tests into test cases and fixtures'',
		#parentIds : [
			''182da1ea32103134e85f73641f926dfe073019bb''
		]
	},
	HiExampleCommit {
		#id : ''a3c5c83f180b72bf7aff9adaf96543a436d60b6b'',
		#comment : ''Merge pull request #610 from pharo-vcs/pharo-plugin-create-branch\n\nFix for issue #608 and #607.'',
		#parentIds : [
			''1ca89d0781829cfc9fac69fbb5b7f62c44c9c09e'',
			''a7b8465ac7af4be05e18db8a2f2441450867798f''
		]
	},
	HiExampleCommit {
		#id : ''1ca89d0781829cfc9fac69fbb5b7f62c44c9c09e'',
		#comment : ''Merge pull request #633 from guillep/fix/branches/with/slashes\n\nFix issue #611'',
		#parentIds : [
			''191522a930a172a3ef2c2c4e18957900ac8880d6'',
			''876aef26b7c787fad5e149cd45f8358602a56589''
		]
	},
	HiExampleCommit {
		#id : ''191522a930a172a3ef2c2c4e18957900ac8880d6'',
		#comment : ''Merge pull request #635 from guillep/fix/travis-pull-requests\n\nMake PR builds run'',
		#parentIds : [
			''579f353a386af3f0cf6823a213942a4117bab230'',
			''afed8001def104cd7656f778a9fee3b04481b586''
		]
	},
	HiExampleCommit {
		#id : ''afed8001def104cd7656f778a9fee3b04481b586'',
		#comment : ''Remove obsolete tests'',
		#parentIds : [
			''88168d95b6abb1db603a1a29bf293e95346b258c''
		]
	},
	HiExampleCommit {
		#id : ''88168d95b6abb1db603a1a29bf293e95346b258c'',
		#comment : ''Move ci check to setUp'',
		#parentIds : [
			''109e5097449d3a841a3f5ae3e3b2d272a0345d20''
		]
	},
	HiExampleCommit {
		#id : ''109e5097449d3a841a3f5ae3e3b2d272a0345d20'',
		#comment : ''Make tests run both in https and ssh. Ignore ssh if in ci'',
		#parentIds : [
			''b3bb75990291fbe6afbc910f935248dae84347d0''
		]
	},
	HiExampleCommit {
		#id : ''b3bb75990291fbe6afbc910f935248dae84347d0'',
		#comment : ''removed setup of custom key\n'',
		#parentIds : [
			''182da1ea32103134e85f73641f926dfe073019bb''
		]
	},
	HiExampleCommit {
		#id : ''579f353a386af3f0cf6823a213942a4117bab230'',
		#comment : ''fixed repair actions to be shown just when they apply'',
		#parentIds : [
			''182da1ea32103134e85f73641f926dfe073019bb''
		]
	},
	HiExampleCommit {
		#id : ''876aef26b7c787fad5e149cd45f8358602a56589'',
		#comment : ''Add tests for branches with slashes\n - IceCommitish should self-delegate checkout to avoid duplication in subclasses\n - git commit could just use gitref to avoid forging strings\n - extend git unborn branch to have a full gitref\n - extended tests with single remotes to test also branches/with/slashes'',
		#parentIds : [
			''3cfef1e86dab7fdb002513fd24d0a7a3f2176cec''
		]
	},
	HiExampleCommit {
		#id : ''3cfef1e86dab7fdb002513fd24d0a7a3f2176cec'',
		#comment : ''Added basic tests for branches/with/slashes'',
		#parentIds : [
			''952f5486ac62b133cef014dabd029930f2c63529''
		]
	},
	HiExampleCommit {
		#id : ''952f5486ac62b133cef014dabd029930f2c63529'',
		#comment : ''Use correct ref to set upstream'',
		#parentIds : [
			''182da1ea32103134e85f73641f926dfe073019bb''
		]
	},
	HiExampleCommit {
		#id : ''182da1ea32103134e85f73641f926dfe073019bb'',
		#comment : ''add some comments'',
		#parentIds : [
			''4203eaf3da2d66a6c7ea631e8d0c53ea7ad8ff6d''
		]
	},
	HiExampleCommit {
		#id : ''4203eaf3da2d66a6c7ea631e8d0c53ea7ad8ff6d'',
		#comment : ''pull repair to appear just when commit is not present'',
		#parentIds : [
			''b53c3a37a77f0c6e614ae7ab2901c82c4b45f086''
		]
	},
	HiExampleCommit {
		#id : ''b53c3a37a77f0c6e614ae7ab2901c82c4b45f086'',
		#comment : ''Merge pull request #632 from pharo-vcs/fix/travis\n\nFix Travis CI'',
		#parentIds : [
			''f10f3c38ab4454b8be4884ba7ae5801a22b53336'',
			''be3f5b98aea2b5a9f4b72d9ec229273ef52d45ca''
		]
	},
	HiExampleCommit {
		#id : ''be3f5b98aea2b5a9f4b72d9ec229273ef52d45ca'',
		#comment : ''Setting up ssh keys\n'',
		#parentIds : [
			''3b6f9ac9021e3a9c8321f8e036e61797d28a6701''
		]
	},
	HiExampleCommit {
		#id : ''3b6f9ac9021e3a9c8321f8e036e61797d28a6701'',
		#comment : ''More robust teardown'',
		#parentIds : [
			''e4c370af7860c2be3b0d3a38e270de26fb3242e9''
		]
	},
	HiExampleCommit {
		#id : ''e4c370af7860c2be3b0d3a38e270de26fb3242e9'',
		#comment : ''Ensure Libgit is correctly installed after reinstalled\n- Shutdown libgit\n- Metacello integration to true\n'',
		#parentIds : [
			''f4756e1506355e968de5fd849c30c24f743d34cb''
		]
	},
	HiExampleCommit {
		#id : ''f4756e1506355e968de5fd849c30c24f743d34cb'',
		#comment : ''libgcrypt is already in osx slaves\n'',
		#parentIds : [
			''02adea76176bbd60fc52811bb1cf255a35e93e23''
		]
	},
	HiExampleCommit {
		#id : ''f10f3c38ab4454b8be4884ba7ae5801a22b53336'',
		#comment : ''Merge pull request #631 from pharo-vcs/dev-0.7\n\nrelease 0.7.3'',
		#parentIds : [
			''8ce94ba3f4365d71501b99286be48caf966dfbf6'',
			''02adea76176bbd60fc52811bb1cf255a35e93e23''
		]
	},
	HiExampleCommit {
		#id : ''02adea76176bbd60fc52811bb1cf255a35e93e23'',
		#comment : ''fetching commits when adding pharo, to be sure it will be found (to avoid unknown commit)'',
		#parentIds : [
			''2d4ac06ad5117c06be6ade6b3cedb8872aa98e30''
		]
	},
	HiExampleCommit {
		#id : ''2d4ac06ad5117c06be6ade6b3cedb8872aa98e30'',
		#comment : ''added "fetch" repair strategy  to act when you do not have the commit in your repository.\nadded "pull" repair strategy to act when your image is forward the disk repository,'',
		#parentIds : [
			''6ad5cc3fc01026899b4e58fc7bafecfa7d6510e4''
		]
	},
	HiExampleCommit {
		#id : ''6ad5cc3fc01026899b4e58fc7bafecfa7d6510e4'',
		#comment : ''Merge 847ce49731373d800cd10786f503aa4a73b53449'',
		#parentIds : [
			''3031cd120084620a5be02d1c161370612fee3472'',
			''847ce49731373d800cd10786f503aa4a73b53449''
		]
	},
	HiExampleCommit {
		#id : ''3031cd120084620a5be02d1c161370612fee3472'',
		#comment : ''added missinfg method'',
		#parentIds : [
			''0b4c43a23e677b2e8358c92bff1f480997ac096d''
		]
	},
	HiExampleCommit {
		#id : ''8ce94ba3f4365d71501b99286be48caf966dfbf6'',
		#comment : ''Merge pull request #627 from pharo-vcs/dev-0.7\n\nremove a test that was making glamour test fail'',
		#parentIds : [
			''07d7e19f4cf9c15d7807d93cd9846f77bdaa2663'',
			''847ce49731373d800cd10786f503aa4a73b53449''
		]
	},
	HiExampleCommit {
		#id : ''847ce49731373d800cd10786f503aa4a73b53449'',
		#comment : ''Merge fe0ce5d2146c17dda6bd6d11a246f1437cc43118'',
		#parentIds : [
			''6b2ad8eab28d36f87961066177d4c0638a2b1060'',
			''fe0ce5d2146c17dda6bd6d11a246f1437cc43118''
		]
	},
	HiExampleCommit {
		#id : ''6b2ad8eab28d36f87961066177d4c0638a2b1060'',
		#comment : ''Remove cancelled method.\nIt causes a GLM test to fail.'',
		#parentIds : [
			''8577b09ea942705f818341725c1e34846b5f6f11''
		]
	},
	HiExampleCommit {
		#id : ''0b4c43a23e677b2e8358c92bff1f480997ac096d'',
		#comment : ''defer default statusbar height to implementation (in case I want to change it, that\''s a better place)'',
		#parentIds : [
			''fe0ce5d2146c17dda6bd6d11a246f1437cc43118''
		]
	},
	HiExampleCommit {
		#id : ''07d7e19f4cf9c15d7807d93cd9846f77bdaa2663'',
		#comment : ''Merge pull request #626 from pharo-vcs/dev-0.7\n\nrevert wrongly removed class'',
		#parentIds : [
			''1b882aa05ff30d941fd4144f2f6454e4bd0a0e88'',
			''fe0ce5d2146c17dda6bd6d11a246f1437cc43118''
		]
	},
	HiExampleCommit {
		#id : ''fe0ce5d2146c17dda6bd6d11a246f1437cc43118'',
		#comment : ''revert wrongly removed class'',
		#parentIds : [
			''59dc466e639f7e0d65d08bdc4a75b6b9f2ed8a69''
		]
	},
	HiExampleCommit {
		#id : ''1b882aa05ff30d941fd4144f2f6454e4bd0a0e88'',
		#comment : ''Merge pull request #621 from pharo-vcs/dev-0.7\n\nreveriting usage of MetacelloCypressBaselineProject because is breakiâ\u0080¦'',
		#parentIds : [
			''d9603be959fa6fd40abec0d4493b1e7b6f566436'',
			''59dc466e639f7e0d65d08bdc4a75b6b9f2ed8a69''
		]
	},
	HiExampleCommit {
		#id : ''59dc466e639f7e0d65d08bdc4a75b6b9f2ed8a69'',
		#comment : ''reveriting usage of MetacelloCypressBaselineProject because is breaking the bootstrap'',
		#parentIds : [
			''a58108fbb7dd2d16ea2aea2362201f5cebf92492''
		]
	},
	HiExampleCommit {
		#id : ''d9603be959fa6fd40abec0d4493b1e7b6f566436'',
		#comment : ''Merge pull request #620 from pharo-vcs/dev-0.7\n\nfix tests'',
		#parentIds : [
			''0f69db17870c3180cd4f51314869484b0f9efe7a'',
			''a58108fbb7dd2d16ea2aea2362201f5cebf92492''
		]
	},
	HiExampleCommit {
		#id : ''a58108fbb7dd2d16ea2aea2362201f5cebf92492'',
		#comment : ''fix tests'',
		#parentIds : [
			''bbffa5b40ed97083285bd11989c9d7ec1c752016''
		]
	},
	HiExampleCommit {
		#id : ''0f69db17870c3180cd4f51314869484b0f9efe7a'',
		#comment : ''Merge pull request #618 from pharo-vcs/dev-0.7\n\nv0.7.1'',
		#parentIds : [
			''0b9771c18b2ff1317b7997f7b2a699911bfbba01'',
			''bbffa5b40ed97083285bd11989c9d7ec1c752016''
		]
	},
	HiExampleCommit {
		#id : ''bbffa5b40ed97083285bd11989c9d7ec1c752016'',
		#comment : ''remove obsoletes$'',
		#parentIds : [
			''ac0b5dc17042cdb4ccd35ff22418eb97d90834c5''
		]
	},
	HiExampleCommit {
		#id : ''ac0b5dc17042cdb4ccd35ff22418eb97d90834c5'',
		#comment : ''remove wrongly remaining class'',
		#parentIds : [
			''e9c33995286158b55eb8a48dcc9fee1a52edacad''
		]
	},
	HiExampleCommit {
		#id : ''e9c33995286158b55eb8a48dcc9fee1a52edacad'',
		#comment : ''Merge pull request #588 from jecisc/cf_useCypressSupportIfAvailable\n\nUse Matecallo Cypress support if available in the baseline.'',
		#parentIds : [
			''46a98f8eba4d059b6ef696a2d1ba28c579146018'',
			''7780c6e9128df259a88909020ddc8d7b2ed70772''
		]
	},
	HiExampleCommit {
		#id : ''46a98f8eba4d059b6ef696a2d1ba28c579146018'',
		#comment : ''Merge pull request #617 from pharo-vcs/fix-undeclared\n\nFix issue #616'',
		#parentIds : [
			''8577b09ea942705f818341725c1e34846b5f6f11'',
			''c3c30a7bcd0446fdd9764eed4bb05f0da9f22ac7''
		]
	},
	HiExampleCommit {
		#id : ''0b9771c18b2ff1317b7997f7b2a699911bfbba01'',
		#comment : ''update current iceberg version'',
		#parentIds : [
			''065eba9a90053fe09e2d51fa69ca649761c146dc''
		]
	},
	HiExampleCommit {
		#id : ''c3c30a7bcd0446fdd9764eed4bb05f0da9f22ac7'',
		#comment : ''Fix undeclareds\n - removed methods with undeclared variables\n - moved IceRemoteNotFoundError to Iceberg package'',
		#parentIds : [
			''8577b09ea942705f818341725c1e34846b5f6f11''
		]
	},
	HiExampleCommit {
		#id : ''065eba9a90053fe09e2d51fa69ca649761c146dc'',
		#comment : ''formating'',
		#parentIds : [
			''e0045fbbda89af96e9d054c53cade5c818909a67''
		]
	},
	HiExampleCommit {
		#id : ''e0045fbbda89af96e9d054c53cade5c818909a67'',
		#comment : ''fix update for Pharo 6.1 and remove 5 min tutorial (is no longer available)'',
		#parentIds : [
			''46d054a467989bdd0a1430749a62c5e2961b2d8b''
		]
	},
	HiExampleCommit {
		#id : ''46d054a467989bdd0a1430749a62c5e2961b2d8b'',
		#comment : ''t push origin master:Merge branch \''dev-0.7\''\n'',
		#parentIds : [
			''9026538b34fd6d343011f83ecf895e471050fec1'',
			''8577b09ea942705f818341725c1e34846b5f6f11''
		]
	},
	HiExampleCommit {
		#id : ''8577b09ea942705f818341725c1e34846b5f6f11'',
		#comment : ''Merge branch \''master\'' into dev-0.7\n\n# Conflicts:\n#\tREADME.md\n'',
		#parentIds : [
			''1f2bbcf7d08896cc12fd6de10731a796ec352643'',
			''9026538b34fd6d343011f83ecf895e471050fec1''
		]
	},
	HiExampleCommit {
		#id : ''1f2bbcf7d08896cc12fd6de10731a796ec352643'',
		#comment : ''Test Metacello baseline installation should install packages clean.'',
		#parentIds : [
			''f9a9003348b8b256cd011a9611d8bed13d569263''
		]
	},
	HiExampleCommit {
		#id : ''f9a9003348b8b256cd011a9611d8bed13d569263'',
		#comment : ''oops, missing announcement of task done.$'',
		#parentIds : [
			''28a156e30fba26dce1ee47254d9a0b4b59fc1513''
		]
	},
	HiExampleCommit {
		#id : ''28a156e30fba26dce1ee47254d9a0b4b59fc1513'',
		#comment : ''fixes #612\ninstalling with metacello does not dirties packages anymore (unless they need to be dirty).\nit also introduces some mechanism to not suspend all iceberg announcements when updating a repo (suspending just the announcements of that repo)'',
		#parentIds : [
			''e46b197e80327f2d4de6bbfe1659e4f50b1cd798''
		]
	},
	HiExampleCommit {
		#id : ''e46b197e80327f2d4de6bbfe1659e4f50b1cd798'',
		#comment : ''fix toolbar refresh on repository browser (it was missing)'',
		#parentIds : [
			''810ac94bf6bd634dd26c5ac7f67c59bccf2492e9''
		]
	},
	HiExampleCommit {
		#id : ''810ac94bf6bd634dd26c5ac7f67c59bccf2492e9'',
		#comment : ''fix settings for iceberg (Iceberg-libgit settings were missing) $'',
		#parentIds : [
			''5132e549580e3a12cbb23a38202268f4e50cbbeb''
		]
	},
	HiExampleCommit {
		#id : ''5132e549580e3a12cbb23a38202268f4e50cbbeb'',
		#comment : ''revparse should work for tags also'',
		#parentIds : [
			''4104a149ef1860be40ec7a1184fe3a6c7649f9db''
		]
	},
	HiExampleCommit {
		#id : ''4104a149ef1860be40ec7a1184fe3a6c7649f9db'',
		#comment : ''Added metacello integration tests\n - be able to switch to a remote branch\n - working copy unload should unload only loaded packages.'',
		#parentIds : [
			''21a3ee74aa7d35ee627dc009142f8052cb3dc6f8''
		]
	},
	HiExampleCommit {
		#id : ''21a3ee74aa7d35ee627dc009142f8052cb3dc6f8'',
		#comment : ''Ice repository creator should be able to checkout any commitish\n - add support for revparse in the repository'',
		#parentIds : [
			''b5ec0147f0957996c41bb6f2347b43e12f3399a8''
		]
	},
	HiExampleCommit {
		#id : ''a7b8465ac7af4be05e18db8a2f2441450867798f'',
		#comment : ''Fix for issue #608 and #607.\n - refactor IcePharoFogbugzModel to follow other checkout branch panels\n - make it pluggable as a plugin'',
		#parentIds : [
			''b5ec0147f0957996c41bb6f2347b43e12f3399a8''
		]
	},
	HiExampleCommit {
		#id : ''b5ec0147f0957996c41bb6f2347b43e12f3399a8'',
		#comment : ''Make tests go green\n - refactor IceUnbornBranch to have one subclass per backend$'',
		#parentIds : [
			''808ae82be728a04921e5d61a0308b8686178c485''
		]
	},
	HiExampleCommit {
		#id : ''808ae82be728a04921e5d61a0308b8686178c485'',
		#comment : ''Depending on Libgit 1.2.0'',
		#parentIds : [
			''fc699935621edf3a04da0b1db51d7b57dcc87391''
		]
	},
	HiExampleCommit {
		#id : ''fc699935621edf3a04da0b1db51d7b57dcc87391'',
		#comment : ''comments$'',
		#parentIds : [
			''e7d897bb5696da1759da0c68973f9d49d965390e''
		]
	},
	HiExampleCommit {
		#id : ''e7d897bb5696da1759da0c68973f9d49d965390e'',
		#comment : ''clean up$'',
		#parentIds : [
			''a5e5abdeef95c03115ff8623ae3dd6170166047b''
		]
	},
	HiExampleCommit {
		#id : ''a5e5abdeef95c03115ff8623ae3dd6170166047b'',
		#comment : ''fix baseline to load also metacello and pharo plugins (for some reason I forget to add them at first)'',
		#parentIds : [
			''517bfa8fdd8106a1b2a14bf07dea4c7235ee469a''
		]
	},
	HiExampleCommit {
		#id : ''517bfa8fdd8106a1b2a14bf07dea4c7235ee469a'',
		#comment : ''add a convenience method$'',
		#parentIds : [
			''ab11df787d6e1fda86ad0ad3a8a0d63393341d7a''
		]
	},
	HiExampleCommit {
		#id : ''ab11df787d6e1fda86ad0ad3a8a0d63393341d7a'',
		#comment : ''fix: remove wrong requirement (baselines can\''t require)'',
		#parentIds : [
			''09b2fd98070a21f1d55120a8246b9e4e1d03e38e''
		]
	},
	HiExampleCommit {
		#id : ''09b2fd98070a21f1d55120a8246b9e4e1d03e38e'',
		#comment : ''using Commander 0.6.1'',
		#parentIds : [
			''2450b6c9f7b687ea2daea6ddc1411c15cb4c3ead''
		]
	},
	HiExampleCommit {
		#id : ''2450b6c9f7b687ea2daea6ddc1411c15cb4c3ead'',
		#comment : ''add "browse merge history". Is not perfect but it allows digging into merges :)'',
		#parentIds : [
			''e84849ec3abfc05c3352ac55d20a4411b2f53c2b''
		]
	},
	HiExampleCommit {
		#id : ''e84849ec3abfc05c3352ac55d20a4411b2f53c2b'',
		#comment : ''Merge 76c06c89e6c7cfd55c2a9d36e445072d33f71055'',
		#parentIds : [
			''3f5e50899aa03374e4e5c238ed67e97cb4b5268c'',
			''76c06c89e6c7cfd55c2a9d36e445072d33f71055''
		]
	},
	HiExampleCommit {
		#id : ''3f5e50899aa03374e4e5c238ed67e97cb4b5268c'',
		#comment : ''renamed because like this is easier to understand :)'',
		#parentIds : [
			''765546cb2f919a13d05d1c907828cad1d342de02''
		]
	},
	HiExampleCommit {
		#id : ''765546cb2f919a13d05d1c907828cad1d342de02'',
		#comment : ''api for forget and remove unified since is more or less the same (and like this is less confusing)'',
		#parentIds : [
			''3a3f5b876f52e941ddaf874dd2e604b58a69bae5''
		]
	},
	HiExampleCommit {
		#id : ''76c06c89e6c7cfd55c2a9d36e445072d33f71055'',
		#comment : ''Remove #incomingCommitsFrom: from repository'',
		#parentIds : [
			''3a3f5b876f52e941ddaf874dd2e604b58a69bae5''
		]
	},
	HiExampleCommit {
		#id : ''3a3f5b876f52e941ddaf874dd2e604b58a69bae5'',
		#comment : ''Cleanups\n - withRepoDo: is not used anymore'',
		#parentIds : [
			''68ffd42fe08f0875b111e2483c1ddf7f4960b2b2''
		]
	},
	HiExampleCommit {
		#id : ''68ffd42fe08f0875b111e2483c1ddf7f4960b2b2'',
		#comment : ''Cleanups and categorizations'',
		#parentIds : [
			''0acae78960e2f1f364b6ded58ca0db7f00dfdbdd''
		]
	},
	HiExampleCommit {
		#id : ''0acae78960e2f1f364b6ded58ca0db7f00dfdbdd'',
		#comment : ''Checking out an unborn branch'',
		#parentIds : [
			''009b932c0eabaeb113d3462d1431ba9527a5632b''
		]
	},
	HiExampleCommit {
		#id : ''009b932c0eabaeb113d3462d1431ba9527a5632b'',
		#comment : ''History view works with unborn'',
		#parentIds : [
			''a0b1abbeefd3f0750ea7611c805d3a0fb61a644f''
		]
	},
	HiExampleCommit {
		#id : ''a0b1abbeefd3f0750ea7611c805d3a0fb61a644f'',
		#comment : ''Merge 7e02fd44bc0fa463a213a266f1d4a07a81254b4d'',
		#parentIds : [
			''1fadb16ad0e2caee7691d9d0b4dab8c57d814a94'',
			''7e02fd44bc0fa463a213a266f1d4a07a81254b4d''
		]
	},
	HiExampleCommit {
		#id : ''1fadb16ad0e2caee7691d9d0b4dab8c57d814a94'',
		#comment : ''Adding tests for new repositories'',
		#parentIds : [
			''c99f8c99e733d2a44a1a7a80a3e025d9c6394b00''
		]
	},
	HiExampleCommit {
		#id : ''7e02fd44bc0fa463a213a266f1d4a07a81254b4d'',
		#comment : ''modify initial extent of repair dialog'',
		#parentIds : [
			''ceb30f8e4ca1cb6fd475cee84a1c2ce615986a79''
		]
	},
	HiExampleCommit {
		#id : ''ceb30f8e4ca1cb6fd475cee84a1c2ce615986a79'',
		#comment : ''Merge 1d980110fcb5197524f5569b71065a425d2b5934'',
		#parentIds : [
			''ba7eab186ce897342e6b49f88a6fd2d71b2e983e'',
			''1d980110fcb5197524f5569b71065a425d2b5934''
		]
	},
	HiExampleCommit {
		#id : ''ba7eab186ce897342e6b49f88a6fd2d71b2e983e'',
		#comment : ''update update script\n'',
		#parentIds : [
			''8841c212550d8c1c947ec95ea1deee06e5292776''
		]
	},
	HiExampleCommit {
		#id : ''8841c212550d8c1c947ec95ea1deee06e5292776'',
		#comment : ''commander also requires Spec70Compatibility now (how is done with Calypso?)'',
		#parentIds : [
			''148db5c0efc7f834cce38e1a20d96c80c40d47bf''
		]
	},
	HiExampleCommit {
		#id : ''1d980110fcb5197524f5569b71065a425d2b5934'',
		#comment : ''Merge 148db5c0efc7f834cce38e1a20d96c80c40d47bf'',
		#parentIds : [
			''740fa6c037c04056901d4bf1da6318cc7ea22449'',
			''148db5c0efc7f834cce38e1a20d96c80c40d47bf''
		]
	},
	HiExampleCommit {
		#id : ''740fa6c037c04056901d4bf1da6318cc7ea22449'',
		#comment : ''Fixing some errors when the repository is empty.\nMoving the default option (double click) to the top of the menu.'',
		#parentIds : [
			''f9efa76ffff7b596ea02285f662ffa40ef1602aa''
		]
	},
	HiExampleCommit {
		#id : ''148db5c0efc7f834cce38e1a20d96c80c40d47bf'',
		#comment : ''restore wrongly removed method'',
		#parentIds : [
			''488455433db6a3a18afb9f8795349900f58ac114''
		]
	},
	HiExampleCommit {
		#id : ''488455433db6a3a18afb9f8795349900f58ac114'',
		#comment : ''Merge c99f8c99e733d2a44a1a7a80a3e025d9c6394b00'',
		#parentIds : [
			''68071976aa00099120a8f488fafd85a9797b30cf'',
			''c99f8c99e733d2a44a1a7a80a3e025d9c6394b00''
		]
	},
	HiExampleCommit {
		#id : ''68071976aa00099120a8f488fafd85a9797b30cf'',
		#comment : ''restore wrongly removed method'',
		#parentIds : [
			''4506e1e58db7f2a99fe3c0912c0efbde2d848f55''
		]
	},
	HiExampleCommit {
		#id : ''c99f8c99e733d2a44a1a7a80a3e025d9c6394b00'',
		#comment : ''Merge 4506e1e58db7f2a99fe3c0912c0efbde2d848f55'',
		#parentIds : [
			''c1bfee0867f8800e9ee41ee36f205bdc25950525'',
			''4506e1e58db7f2a99fe3c0912c0efbde2d848f55''
		]
	},
	HiExampleCommit {
		#id : ''c1bfee0867f8800e9ee41ee36f205bdc25950525'',
		#comment : ''Fix #605. Requires `KMRepository reset` afterwards'',
		#parentIds : [
			''f9efa76ffff7b596ea02285f662ffa40ef1602aa''
		]
	},
	HiExampleCommit {
		#id : ''4506e1e58db7f2a99fe3c0912c0efbde2d848f55'',
		#comment : ''new fix add dependences for TipUI for pharo6 '',
		#parentIds : [
			''2f23b3aa50ac20658858973eaee26a43a18c2b65''
		]
	},
	HiExampleCommit {
		#id : ''2f23b3aa50ac20658858973eaee26a43a18c2b65'',
		#comment : ''Merge f9efa76ffff7b596ea02285f662ffa40ef1602aa'',
		#parentIds : [
			''149da272c2fc2999a09c1e5262ed376889575418'',
			''f9efa76ffff7b596ea02285f662ffa40ef1602aa''
		]
	},
	HiExampleCommit {
		#id : ''149da272c2fc2999a09c1e5262ed376889575418'',
		#comment : ''new fix add dependences for TipUI for pharo6 '',
		#parentIds : [
			''e9c548a314ffb227405d4a19c897e43f194c2835''
		]
	},
	HiExampleCommit {
		#id : ''f9efa76ffff7b596ea02285f662ffa40ef1602aa'',
		#comment : ''Fixes #604'',
		#parentIds : [
			''30c647d70671ab42663e995dc04e8726f075ae40''
		]
	},
	HiExampleCommit {
		#id : ''30c647d70671ab42663e995dc04e8726f075ae40'',
		#comment : ''Merge e9c548a314ffb227405d4a19c897e43f194c2835'',
		#parentIds : [
			''5c7ccb224c4f6d002dfab9040925fdb78defc5bf'',
			''e9c548a314ffb227405d4a19c897e43f194c2835''
		]
	},
	HiExampleCommit {
		#id : ''5c7ccb224c4f6d002dfab9040925fdb78defc5bf'',
		#comment : ''Fixes #604'',
		#parentIds : [
			''d546d30760ef2f1e0788c90fc4ec83148d694ad3''
		]
	},
	HiExampleCommit {
		#id : ''e9c548a314ffb227405d4a19c897e43f194c2835'',
		#comment : ''fix spec70 path and make baseline depend on a release version'',
		#parentIds : [
			''2832c729569b764ae89672fd1959e3341a693b7d''
		]
	},
	HiExampleCommit {
		#id : ''2832c729569b764ae89672fd1959e3341a693b7d'',
		#comment : ''Merge d546d30760ef2f1e0788c90fc4ec83148d694ad3'',
		#parentIds : [
			''7b5afceebaf64e06878bb3f2b6410fe6078a6e8c'',
			''d546d30760ef2f1e0788c90fc4ec83148d694ad3''
		]
	},
	HiExampleCommit {
		#id : ''7b5afceebaf64e06878bb3f2b6410fe6078a6e8c'',
		#comment : ''1000 is too much, 700 is enough to show all '',
		#parentIds : [
			''266470a614b00e16655cf2a56f2e0d961be137bd''
		]
	},
	HiExampleCommit {
		#id : ''d546d30760ef2f1e0788c90fc4ec83148d694ad3'',
		#comment : ''Avoid rendering pull and push in case of missing repository.'',
		#parentIds : [
			''99b598f0921a79e7518215d2e7c518b3a5d903cd''
		]
	},
	HiExampleCommit {
		#id : ''266470a614b00e16655cf2a56f2e0d961be137bd'',
		#comment : ''for some reason IceTipCommitBrowser was not suscribing to refresh'',
		#parentIds : [
			''b8de6621a56675bc0d24edaa9b86ad1b512ad942''
		]
	},
	HiExampleCommit {
		#id : ''b8de6621a56675bc0d24edaa9b86ad1b512ad942'',
		#comment : ''pahro6 needs to load spec compatibility'',
		#parentIds : [
			''b3ba7548f1107f0ee67231c0a5068ec4b53e7be2''
		]
	},
	HiExampleCommit {
		#id : ''99b598f0921a79e7518215d2e7c518b3a5d903cd'',
		#comment : ''Adding independent setting for Github username and password.\nAdding delete repository from disk.$'',
		#parentIds : [
			''f80f9e6b3e6d33c0b7ed8d9c5a0ecc8421ab51e0''
		]
	},
	HiExampleCommit {
		#id : ''f80f9e6b3e6d33c0b7ed8d9c5a0ecc8421ab51e0'',
		#comment : ''Merge b3ba7548f1107f0ee67231c0a5068ec4b53e7be2'',
		#parentIds : [
			''bb3bd5b3cd3c017527ab900c8286c0c6eea81442'',
			''b3ba7548f1107f0ee67231c0a5068ec4b53e7be2''
		]
	},
	HiExampleCommit {
		#id : ''bb3bd5b3cd3c017527ab900c8286c0c6eea81442'',
		#comment : ''Handling correctly the authentication to the GIthub API. \nAdding retries and handling cancel.'',
		#parentIds : [
			''c94101fccc5630df9994556938248d5dff7ffe4b''
		]
	},
	HiExampleCommit {
		#id : ''b3ba7548f1107f0ee67231c0a5068ec4b53e7be2'',
		#comment : ''Remove unnecessary users of validateCanMerge'',
		#parentIds : [
			''d1df88dbeb9cb709e42199a5a9538b26a98d8d9a''
		]
	},
	HiExampleCommit {
		#id : ''d1df88dbeb9cb709e42199a5a9538b26a98d8d9a'',
		#comment : ''Merge e2587a979bf3321858381471deb79ed02ab58220'',
		#parentIds : [
			''847e5ff32fd82a9335daf46299a13467d09fb9f2'',
			''e2587a979bf3321858381471deb79ed02ab58220''
		]
	},
	HiExampleCommit {
		#id : ''847e5ff32fd82a9335daf46299a13467d09fb9f2'',
		#comment : ''Change error by warning'',
		#parentIds : [
			''303df6c6c86a6ccf316b891f3966705067b41eba''
		]
	},
	HiExampleCommit {
		#id : ''e2587a979bf3321858381471deb79ed02ab58220'',
		#comment : ''Merge cf5a05aa9548546a7cb99a5b22001c5069a7b9ad'',
		#parentIds : [
			''a2d9998c609d2ed462c0fe71e7517a6e7b91b127'',
			''cf5a05aa9548546a7cb99a5b22001c5069a7b9ad''
		]
	},
	HiExampleCommit {
		#id : ''a2d9998c609d2ed462c0fe71e7517a6e7b91b127'',
		#comment : ''renamed #model with #repository (is not a model anymore, is the entity)\nfix remaining methods'',
		#parentIds : [
			''007661f4f17d804f2e399bb98401903495b47952''
		]
	},
	HiExampleCommit {
		#id : ''007661f4f17d804f2e399bb98401903495b47952'',
		#comment : ''model is already a repository'',
		#parentIds : [
			''627a8470d1b6b76dce9815163ede1f3c1ee8e544''
		]
	},
	HiExampleCommit {
		#id : ''627a8470d1b6b76dce9815163ede1f3c1ee8e544'',
		#comment : ''fix wrong commit$'',
		#parentIds : [
			''80e33d14653e5b354b932bba4ced564245a7661d''
		]
	},
	HiExampleCommit {
		#id : ''80e33d14653e5b354b932bba4ced564245a7661d'',
		#comment : ''should not use directly an entity into a panel'',
		#parentIds : [
			''87b4424dd9ab40503cd1220ad58469e9beb67376''
		]
	},
	HiExampleCommit {
		#id : ''87b4424dd9ab40503cd1220ad58469e9beb67376'',
		#comment : ''in pharo6 there is still Object>>#name so this request will  not be redirected as expected (so we\''ll do it explicitly)'',
		#parentIds : [
			''bba244cd95a1909d7e47b1a8fd79e69136b4099a''
		]
	},
	HiExampleCommit {
		#id : ''303df6c6c86a6ccf316b891f3966705067b41eba'',
		#comment : ''Fix for branch>>#push'',
		#parentIds : [
			''c1883389236be99fb51d2f498ea1e89678540349''
		]
	},
	HiExampleCommit {
		#id : ''c1883389236be99fb51d2f498ea1e89678540349'',
		#comment : ''Create merge commit with files'',
		#parentIds : [
			''cf5a05aa9548546a7cb99a5b22001c5069a7b9ad''
		]
	},
	HiExampleCommit {
		#id : ''bba244cd95a1909d7e47b1a8fd79e69136b4099a'',
		#comment : ''add badgeColors for dark theme on pharo6.1'',
		#parentIds : [
			''019f76f183b8439e378934c1fb4c5cf5489e76ec''
		]
	},
	HiExampleCommit {
		#id : ''cf5a05aa9548546a7cb99a5b22001c5069a7b9ad'',
		#comment : ''Merge 019f76f183b8439e378934c1fb4c5cf5489e76ec'',
		#parentIds : [
			''1550ca6f33a06e112929c4d521d4e822cb78c949'',
			''019f76f183b8439e378934c1fb4c5cf5489e76ec''
		]
	},
	HiExampleCommit {
		#id : ''1550ca6f33a06e112929c4d521d4e822cb78c949'',
		#comment : ''Add better help and initial extent in repair actions'',
		#parentIds : [
			''4b4165c55e5f51de44ccfc324983cbd25bfad9bd''
		]
	},
	HiExampleCommit {
		#id : ''019f76f183b8439e378934c1fb4c5cf5489e76ec'',
		#comment : ''Merge 4b4165c55e5f51de44ccfc324983cbd25bfad9bd'',
		#parentIds : [
			''852affbfda30f33baf8de5b7a2941fb3c0ef2ac6'',
			''4b4165c55e5f51de44ccfc324983cbd25bfad9bd''
		]
	},
	HiExampleCommit {
		#id : ''4b4165c55e5f51de44ccfc324983cbd25bfad9bd'',
		#comment : ''Merge c94101fccc5630df9994556938248d5dff7ffe4b'',
		#parentIds : [
			''a00a94f3fa4ab3a8b54e850bb9e8a246c01d14f5'',
			''c94101fccc5630df9994556938248d5dff7ffe4b''
		]
	},
	HiExampleCommit {
		#id : ''a00a94f3fa4ab3a8b54e850bb9e8a246c01d14f5'',
		#comment : ''Enhancing refresh of new branch creation$'',
		#parentIds : [
			''1b83525ff2563077a62d1a74060e327ce09679be''
		]
	},
	HiExampleCommit {
		#id : ''c94101fccc5630df9994556938248d5dff7ffe4b'',
		#comment : ''Updating the libgit2 to use the latest release, including reset and index operations\n'',
		#parentIds : [
			''1b83525ff2563077a62d1a74060e327ce09679be''
		]
	},
	HiExampleCommit {
		#id : ''1b83525ff2563077a62d1a74060e327ce09679be'',
		#comment : ''Remove unused temp'',
		#parentIds : [
			''ba46a20144292f173187564d2b5179dad26c6861''
		]
	},
	HiExampleCommit {
		#id : ''ba46a20144292f173187564d2b5179dad26c6861'',
		#comment : ''Merge 40df0ecdc104eb2062e8df4ed0c7428773e0138b'',
		#parentIds : [
			''9c2269e1d8a786085dfe8559db9b5ea738d30381'',
			''40df0ecdc104eb2062e8df4ed0c7428773e0138b''
		]
	},
	HiExampleCommit {
		#id : ''40df0ecdc104eb2062e8df4ed0c7428773e0138b'',
		#comment : ''Creating new branch should not make working copy dirty'',
		#parentIds : [
			''185ab80a58042ac7feda36f0eb872b93cff66aaf''
		]
	},
	HiExampleCommit {
		#id : ''9c2269e1d8a786085dfe8559db9b5ea738d30381'',
		#comment : ''Refresh when new branch is created'',
		#parentIds : [
			''185ab80a58042ac7feda36f0eb872b93cff66aaf''
		]
	},
	HiExampleCommit {
		#id : ''852affbfda30f33baf8de5b7a2941fb3c0ef2ac6'',
		#comment : ''no menu if there is no item selected'',
		#parentIds : [
			''80177223287e0bd21af3a246c227ccda4422eebe''
		]
	},
	HiExampleCommit {
		#id : ''80177223287e0bd21af3a246c227ccda4422eebe'',
		#comment : ''Merge 185ab80a58042ac7feda36f0eb872b93cff66aaf'',
		#parentIds : [
			''aab97f785a05df810eddfcf65d15e2ea3c6c0fa4'',
			''185ab80a58042ac7feda36f0eb872b93cff66aaf''
		]
	},
	HiExampleCommit {
		#id : ''185ab80a58042ac7feda36f0eb872b93cff66aaf'',
		#comment : ''Use merge strategy'',
		#parentIds : [
			''6518f936a04d12afaa1aab56e830de665db72935''
		]
	},
	HiExampleCommit {
		#id : ''6518f936a04d12afaa1aab56e830de665db72935'',
		#comment : ''Pass on checkout remote branches$'',
		#parentIds : [
			''be4f245253ecef5784fc21870f658fa007f38502''
		]
	},
	HiExampleCommit {
		#id : ''be4f245253ecef5784fc21870f658fa007f38502'',
		#comment : ''Fixing validate can merge'',
		#parentIds : [
			''00f2de9fe694e3d5aa31b06da1ca226e1a1f4b3a''
		]
	},
	HiExampleCommit {
		#id : ''00f2de9fe694e3d5aa31b06da1ca226e1a1f4b3a'',
		#comment : ''Validate merge'',
		#parentIds : [
			''5d1f3c6a9a809a9e62ad4a347443b5574515fed4''
		]
	},
	HiExampleCommit {
		#id : ''5d1f3c6a9a809a9e62ad4a347443b5574515fed4'',
		#comment : ''Workaround commit and push for branches with no upstream$'',
		#parentIds : [
			''4472edc768f637d1dbf3bfe5f620ebc6f3f5d615''
		]
	},
	HiExampleCommit {
		#id : ''4472edc768f637d1dbf3bfe5f620ebc6f3f5d615'',
		#comment : ''Clean github plugin'',
		#parentIds : [
			''c4b1006b71bf954b01886f688583b24faeb6dc36''
		]
	},
	HiExampleCommit {
		#id : ''c4b1006b71bf954b01886f688583b24faeb6dc36'',
		#comment : ''Putting all plugins together'',
		#parentIds : [
			''ccfbeb447abed72a6e3f48b8ec6dd717b8a14e9c''
		]
	},
	HiExampleCommit {
		#id : ''ccfbeb447abed72a6e3f48b8ec6dd717b8a14e9c'',
		#comment : ''Merge branch \''dev-0.7\'' of github.com:pharo-vcs/iceberg into dev-0.7\n'',
		#parentIds : [
			''89749f3ac8da91cb538471b6012c41e38f6fb263'',
			''fb3a293945be5306e73275e2b5d0fd85d0eb5afe''
		]
	},
	HiExampleCommit {
		#id : ''aab97f785a05df810eddfcf65d15e2ea3c6c0fa4'',
		#comment : ''compatibility with pharo6.1'',
		#parentIds : [
			''8ccc916d584c33aa2908d1ea004ad1b83fe6b161''
		]
	},
	HiExampleCommit {
		#id : ''89749f3ac8da91cb538471b6012c41e38f6fb263'',
		#comment : ''Removing a halt and adding the author in the commit list.'',
		#parentIds : [
			''662a15fd56e805b70cbbb650764177771bcf0d5e''
		]
	},
	HiExampleCommit {
		#id : ''fb3a293945be5306e73275e2b5d0fd85d0eb5afe'',
		#comment : ''Merge 662a15fd56e805b70cbbb650764177771bcf0d5e'',
		#parentIds : [
			''074593570d2c26dafb1125b88e39c42bd7a0c27c'',
			''662a15fd56e805b70cbbb650764177771bcf0d5e''
		]
	},
	HiExampleCommit {
		#id : ''074593570d2c26dafb1125b88e39c42bd7a0c27c'',
		#comment : ''Selecting correct remote for pull/push\n - if branch has upstream use upstream\n - otherwise select first in list'',
		#parentIds : [
			''96737dc82700c179256fd3670a6a7251acc855d5''
		]
	},
	HiExampleCommit {
		#id : ''662a15fd56e805b70cbbb650764177771bcf0d5e'',
		#comment : ''Merge 8ccc916d584c33aa2908d1ea004ad1b83fe6b161'',
		#parentIds : [
			''e3137ad3f96f523759748b406d4e2f24c3609504'',
			''8ccc916d584c33aa2908d1ea004ad1b83fe6b161''
		]
	},
	HiExampleCommit {
		#id : ''e3137ad3f96f523759748b406d4e2f24c3609504'',
		#comment : ''Refactoring to allow to have different points for the commands. \nSeparating them in actions to not have duplicated behavior.'',
		#parentIds : [
			''918797622118a9f54428ce7ac1f001f838277e99''
		]
	},
	HiExampleCommit {
		#id : ''96737dc82700c179256fd3670a6a7251acc855d5'',
		#comment : ''Merge 8ccc916d584c33aa2908d1ea004ad1b83fe6b161'',
		#parentIds : [
			''9c162235701fcdd42760beb2d4c9417eacc1e2de'',
			''8ccc916d584c33aa2908d1ea004ad1b83fe6b161''
		]
	},
	HiExampleCommit {
		#id : ''9c162235701fcdd42760beb2d4c9417eacc1e2de'',
		#comment : ''Fixing UI when no pull remote is there'',
		#parentIds : [
			''533cde908272e54d59234d9d3cb66f6bcccca9c8''
		]
	},
	HiExampleCommit {
		#id : ''533cde908272e54d59234d9d3cb66f6bcccca9c8'',
		#comment : ''Remove push remote'',
		#parentIds : [
			''dfdc87f944ee9cffece468ec3083ac18dbf79082''
		]
	},
	HiExampleCommit {
		#id : ''dfdc87f944ee9cffece468ec3083ac18dbf79082'',
		#comment : ''Remove the idea of pull requests$'',
		#parentIds : [
			''a206cb2037516d6506c7d983446aa5a7185b2dd4''
		]
	},
	HiExampleCommit {
		#id : ''8ccc916d584c33aa2908d1ea004ad1b83fe6b161'',
		#comment : ''revert needs to refresh the commit (because it changed, so we don\''t know what we are commiting now)'',
		#parentIds : [
			''918797622118a9f54428ce7ac1f001f838277e99''
		]
	},
	HiExampleCommit {
		#id : ''918797622118a9f54428ce7ac1f001f838277e99'',
		#comment : ''Fixing to support old pharo versions'',
		#parentIds : [
			''6769b439b5f748e79acb10ab4bb6d905884cd2ae''
		]
	},
	HiExampleCommit {
		#id : ''6769b439b5f748e79acb10ab4bb6d905884cd2ae'',
		#comment : ''FIxing the Pull when there is no origin remote'',
		#parentIds : [
			''ba21d56730c82056dc46f65ba50e13b9fb9a76da''
		]
	},
	HiExampleCommit {
		#id : ''ba21d56730c82056dc46f65ba50e13b9fb9a76da'',
		#comment : ''Merge a206cb2037516d6506c7d983446aa5a7185b2dd4'',
		#parentIds : [
			''6d0caecba548ed07983aace225441ffe5c64b82a'',
			''a206cb2037516d6506c7d983446aa5a7185b2dd4''
		]
	},
	HiExampleCommit {
		#id : ''6d0caecba548ed07983aace225441ffe5c64b82a'',
		#comment : ''Adding the label of the sides in the diff panel (for merge and commit)'',
		#parentIds : [
			''b6233e8719887659f7218eb0a7560b061928915f''
		]
	},
	HiExampleCommit {
		#id : ''a206cb2037516d6506c7d983446aa5a7185b2dd4'',
		#comment : ''Merge 6f1949608007d995e997e641b4f5f90daa5d6042'',
		#parentIds : [
			''ec5e57e26dd9bf6d010c439b23465bc14762bb22'',
			''6f1949608007d995e997e641b4f5f90daa5d6042''
		]
	},
	HiExampleCommit {
		#id : ''ec5e57e26dd9bf6d010c439b23465bc14762bb22'',
		#comment : ''Only notify if package dirtyness changes$'',
		#parentIds : [
			''b6233e8719887659f7218eb0a7560b061928915f''
		]
	},
	HiExampleCommit {
		#id : ''6f1949608007d995e997e641b4f5f90daa5d6042'',
		#comment : ''Merge b6233e8719887659f7218eb0a7560b061928915f'',
		#parentIds : [
			''3c6e63a245129027d6e3974200f1c844e5a38d97'',
			''b6233e8719887659f7218eb0a7560b061928915f''
		]
	},
	HiExampleCommit {
		#id : ''3c6e63a245129027d6e3974200f1c844e5a38d97'',
		#comment : ''add edit repository command'',
		#parentIds : [
			''d37c970df1aec29e5129fa7f24509873d617d449''
		]
	},
	HiExampleCommit {
		#id : ''b6233e8719887659f7218eb0a7560b061928915f'',
		#comment : ''Merge d37c970df1aec29e5129fa7f24509873d617d449'',
		#parentIds : [
			''2bd3dbc710fb552de9f79bfc1cba4489b7908bc8'',
			''d37c970df1aec29e5129fa7f24509873d617d449''
		]
	},
	HiExampleCommit {
		#id : ''2bd3dbc710fb552de9f79bfc1cba4489b7908bc8'',
		#comment : ''Trying to avoid reentrant notifications'',
		#parentIds : [
			''b7e925a47c4a64c93183fe2952eeca107391b0b4''
		]
	},
	HiExampleCommit {
		#id : ''b7e925a47c4a64c93183fe2952eeca107391b0b4'',
		#comment : ''Be able to open history in dettached mode'',
		#parentIds : [
			''6dd41278bd4d384b610bcc8f5e979b10352917af''
		]
	},
	HiExampleCommit {
		#id : ''d37c970df1aec29e5129fa7f24509873d617d449'',
		#comment : ''Merge 6dd41278bd4d384b610bcc8f5e979b10352917af'',
		#parentIds : [
			''cbe544cd9d7036eafe877f0302b620d46310e1bc'',
			''6dd41278bd4d384b610bcc8f5e979b10352917af''
		]
	},
	HiExampleCommit {
		#id : ''cbe544cd9d7036eafe877f0302b620d46310e1bc'',
		#comment : ''add fetch remote command\n\ncloses #599'',
		#parentIds : [
			''cee1e66dca9552c14b373eae0705c2384ef940ec''
		]
	},
	HiExampleCommit {
		#id : ''6dd41278bd4d384b610bcc8f5e979b10352917af'',
		#comment : ''Merge 7c1abb8a05dd23da39f8a022834960bebf2d2558'',
		#parentIds : [
			''9cf8318cd737f2d70a306eca154602b6a9c3145d'',
			''7c1abb8a05dd23da39f8a022834960bebf2d2558''
		]
	},
	HiExampleCommit {
		#id : ''cee1e66dca9552c14b373eae0705c2384ef940ec'',
		#comment : ''- add icons to menu entries\n- refactor'',
		#parentIds : [
			''7c1abb8a05dd23da39f8a022834960bebf2d2558''
		]
	},
	HiExampleCommit {
		#id : ''7c1abb8a05dd23da39f8a022834960bebf2d2558'',
		#comment : ''- restore handling errors that went missing *again* (I hope is not a bug)\n- extend libgit error handling to #fetchBranch:\n- IceTipAction needs to use IceTipInteractiveErrorVisitor'',
		#parentIds : [
			''bd816990fcd45c803ae03d621ff7c9544916994a''
		]
	},
	HiExampleCommit {
		#id : ''9cf8318cd737f2d70a306eca154602b6a9c3145d'',
		#comment : ''Merge bd816990fcd45c803ae03d621ff7c9544916994a'',
		#parentIds : [
			''497ad241ef751971f9c48b4ac7a28660afe518af'',
			''bd816990fcd45c803ae03d621ff7c9544916994a''
		]
	},
	HiExampleCommit {
		#id : ''497ad241ef751971f9c48b4ac7a28660afe518af'',
		#comment : ''Adding checkout and new branch options in commit.'',
		#parentIds : [
			''1552edb371c1bc9509dcc379b3d8b86c196431e6''
		]
	},
	HiExampleCommit {
		#id : ''bd816990fcd45c803ae03d621ff7c9544916994a'',
		#comment : ''Merge 1552edb371c1bc9509dcc379b3d8b86c196431e6'',
		#parentIds : [
			''a80267221d365889f3b41d43cabd015e34501fc6'',
			''1552edb371c1bc9509dcc379b3d8b86c196431e6''
		]
	},
	HiExampleCommit {
		#id : ''a80267221d365889f3b41d43cabd015e34501fc6'',
		#comment : ''metacello plugin working (and some refactor of names and orders)$'',
		#parentIds : [
			''b5349a8d3315ba9d01a52c830b5dd40c991fa4c7''
		]
	},
	HiExampleCommit {
		#id : ''1552edb371c1bc9509dcc379b3d8b86c196431e6'',
		#comment : ''Coherent diff morph panels'',
		#parentIds : [
			''312fcf92b995cb1d94c496201e9c29e7829c0cc3''
		]
	},
	HiExampleCommit {
		#id : ''312fcf92b995cb1d94c496201e9c29e7829c0cc3'',
		#comment : ''Adding the selection of remotes in the GithubPlugin'',
		#parentIds : [
			''df33f4eaf84ed981422d6fd361cd0eacdb6dd1fc''
		]
	},
	HiExampleCommit {
		#id : ''df33f4eaf84ed981422d6fd361cd0eacdb6dd1fc'',
		#comment : ''Should select first remote if no push remotes are selected'',
		#parentIds : [
			''39e94271d233e7ac6efe1c6cc14a32b2afe96433''
		]
	},
	HiExampleCommit {
		#id : ''39e94271d233e7ac6efe1c6cc14a32b2afe96433'',
		#comment : ''Removed duplicated plugins'',
		#parentIds : [
			''ee3f049e9cfb634898891ac0f32ecf8de126a1d2''
		]
	},
	HiExampleCommit {
		#id : ''ee3f049e9cfb634898891ac0f32ecf8de126a1d2'',
		#comment : ''Merge b5349a8d3315ba9d01a52c830b5dd40c991fa4c7'',
		#parentIds : [
			''24b8da7c99c956541f194f9da56dfcbaabf4aaca'',
			''b5349a8d3315ba9d01a52c830b5dd40c991fa4c7''
		]
	},
	HiExampleCommit {
		#id : ''24b8da7c99c956541f194f9da56dfcbaabf4aaca'',
		#comment : ''- Organize plugins\n- Clone from github configures remotes.'',
		#parentIds : [
			''88bbf5aa17754f9b07a96011f1214d9c81e6640f''
		]
	},
	HiExampleCommit {
		#id : ''b5349a8d3315ba9d01a52c830b5dd40c991fa4c7'',
		#comment : ''Merge 88bbf5aa17754f9b07a96011f1214d9c81e6640f'',
		#parentIds : [
			''6a6d400ba21ae303686cc10a99e1a2b4a2fe2403'',
			''88bbf5aa17754f9b07a96011f1214d9c81e6640f''
		]
	},
	HiExampleCommit {
		#id : ''6a6d400ba21ae303686cc10a99e1a2b4a2fe2403'',
		#comment : ''IceCheckoutDoNotLoadPackages was not doing anything. Now it works as expected.'',
		#parentIds : [
			''38cd3d183985a8f63fdfb3e12545360d8610daf2''
		]
	},
	HiExampleCommit {
		#id : ''88bbf5aa17754f9b07a96011f1214d9c81e6640f'',
		#comment : ''Grouping plugins$'',
		#parentIds : [
			''f13d6c3bd6e6ed808f02b46e4172dae437a5eb0d''
		]
	},
	HiExampleCommit {
		#id : ''f13d6c3bd6e6ed808f02b46e4172dae437a5eb0d'',
		#comment : ''Merge ea59dea9860783909d036419e220d69b77826a49'',
		#parentIds : [
			''5312cbfd576070652cdfcd3f76a1801f66ee9dfb'',
			''ea59dea9860783909d036419e220d69b77826a49''
		]
	},
	HiExampleCommit {
		#id : ''5312cbfd576070652cdfcd3f76a1801f66ee9dfb'',
		#comment : ''Recovering Fogbugz Issue plugin'',
		#parentIds : [
			''fcff6dabfe5d4466df47b956f411c9a4556eea00''
		]
	},
	HiExampleCommit {
		#id : ''ea59dea9860783909d036419e220d69b77826a49'',
		#comment : ''Merge branch \''dev-0.7\'' of github.com:pharo-vcs/iceberg into dev-0.7\n'',
		#parentIds : [
			''53286a2239ebdc4ff99ae4ef5ef38e9677a53055'',
			''38cd3d183985a8f63fdfb3e12545360d8610daf2''
		]
	},
	HiExampleCommit {
		#id : ''53286a2239ebdc4ff99ae4ef5ef38e9677a53055'',
		#comment : ''Implementing the Github plugin as IceTip commands.'',
		#parentIds : [
			''37978abdd399eb5bd11145133f5ed4b849c180db''
		]
	},
	HiExampleCommit {
		#id : ''38cd3d183985a8f63fdfb3e12545360d8610daf2'',
		#comment : ''do not use #adoptCommit: since status is correct. Use simply #referenceCommit:'',
		#parentIds : [
			''0226a84576cc4f0860a3f84feec0aea198ea7a41''
		]
	},
	HiExampleCommit {
		#id : ''fcff6dabfe5d4466df47b956f411c9a4556eea00'',
		#comment : ''Add placeholders for new pharo issue command'',
		#parentIds : [
			''5d81401735839fc32447c3f9bc2833e1d6ff3aec''
		]
	},
	HiExampleCommit {
		#id : ''5d81401735839fc32447c3f9bc2833e1d6ff3aec'',
		#comment : ''Add Plugin separator item'',
		#parentIds : [
			''37978abdd399eb5bd11145133f5ed4b849c180db''
		]
	},
	HiExampleCommit {
		#id : ''0226a84576cc4f0860a3f84feec0aea198ea7a41'',
		#comment : ''fix outlines: they do not want proportional layouts'',
		#parentIds : [
			''7e794e37f71f4d57e0518cce798f2e09f56104ac''
		]
	},
	HiExampleCommit {
		#id : ''7e794e37f71f4d57e0518cce798f2e09f56104ac'',
		#comment : ''Merge 37978abdd399eb5bd11145133f5ed4b849c180db'',
		#parentIds : [
			''f1803c456e8469789bf2106c0051e87c01d33253'',
			''37978abdd399eb5bd11145133f5ed4b849c180db''
		]
	},
	HiExampleCommit {
		#id : ''f1803c456e8469789bf2106c0051e87c01d33253'',
		#comment : ''fix package selected when there are no elements. '',
		#parentIds : [
			''2402c5e86923162fb9d500d13b633656f7dffb3b''
		]
	},
	HiExampleCommit {
		#id : ''37978abdd399eb5bd11145133f5ed4b849c180db'',
		#comment : ''Test that index is clean in all merge scenarios'',
		#parentIds : [
			''2402c5e86923162fb9d500d13b633656f7dffb3b''
		]
	},
	HiExampleCommit {
		#id : ''2402c5e86923162fb9d500d13b633656f7dffb3b'',
		#comment : ''Make #refreshPackages work in case of merge'',
		#parentIds : [
			''50e05cdf30160817c8d88f6f311bff9985f33bde''
		]
	},
	HiExampleCommit {
		#id : ''50e05cdf30160817c8d88f6f311bff9985f33bde'',
		#comment : ''Sort changes correctly to not get image changes overwritten'',
		#parentIds : [
			''eeea7e0a0ab7538b6d05708e71821e71ad715558''
		]
	},
	HiExampleCommit {
		#id : ''eeea7e0a0ab7538b6d05708e71821e71ad715558'',
		#comment : ''Fixes in checkout and push browsers'',
		#parentIds : [
			''56616210d973645abc03a25825285c8053e94337''
		]
	},
	HiExampleCommit {
		#id : ''56616210d973645abc03a25825285c8053e94337'',
		#comment : ''Change IcebergTests-Core to IcebergTests-Common'',
		#parentIds : [
			''dcbc2c07230ed930e052b65c08b1b8afa1bf691e''
		]
	},
	HiExampleCommit {
		#id : ''7780c6e9128df259a88909020ddc8d7b2ed70772'',
		#comment : ''Use Matecallo Cypress support if available in the baseline.\nThis support is present in Pharo 6.1 and 7.\nIt should allow to update Iceberg without removing its packages first.\n\nClose: https://github.com/pharo-vcs/iceberg/issues/546'',
		#parentIds : [
			''dcbc2c07230ed930e052b65c08b1b8afa1bf691e''
		]
	},
	HiExampleCommit {
		#id : ''dcbc2c07230ed930e052b65c08b1b8afa1bf691e'',
		#comment : ''make metacello plugin work.'',
		#parentIds : [
			''9ee72e4fd97c1da06ce7b1b28c1fe9fab85d07bd''
		]
	},
	HiExampleCommit {
		#id : ''9ee72e4fd97c1da06ce7b1b28c1fe9fab85d07bd'',
		#comment : ''fix: repositories do not keep anymore packages, workingcopies do.'',
		#parentIds : [
			''f82fdc4ca8727044562ed75848eb3c023568b626''
		]
	},
	HiExampleCommit {
		#id : ''f82fdc4ca8727044562ed75848eb3c023568b626'',
		#comment : ''refresh packages when referenceCommit changes'',
		#parentIds : [
			''bd478fa20dabdb5fea475974e358958a8f2fdb3a''
		]
	},
	HiExampleCommit {
		#id : ''bd478fa20dabdb5fea475974e358958a8f2fdb3a'',
		#comment : ''add error handling for "remote already exists" error.'',
		#parentIds : [
			''1695cca34541b17462f9e617b93024bef72cad9e''
		]
	},
	HiExampleCommit {
		#id : ''1695cca34541b17462f9e617b93024bef72cad9e'',
		#comment : ''add remotes and refactor stuff'',
		#parentIds : [
			''72fa9e34cbd5a09fd21629d4bdb547d8a37e1aa0''
		]
	},
	HiExampleCommit {
		#id : ''72fa9e34cbd5a09fd21629d4bdb547d8a37e1aa0'',
		#comment : ''remove force bold because it does not works anyway'',
		#parentIds : [
			''cf45bb184507a17e0650159cb1228034864902a9''
		]
	},
	HiExampleCommit {
		#id : ''cf45bb184507a17e0650159cb1228034864902a9'',
		#comment : ''plugins promoted to independent packages$'',
		#parentIds : [
			''d12a4346f55d8240580603b77922420e3d21c133''
		]
	},
	HiExampleCommit {
		#id : ''d12a4346f55d8240580603b77922420e3d21c133'',
		#comment : ''plugins promoted to independent packages '',
		#parentIds : [
			''537ac9a62a50acc86f21137a88eb1c80250e7879''
		]
	},
	HiExampleCommit {
		#id : ''537ac9a62a50acc86f21137a88eb1c80250e7879'',
		#comment : ''Merge 5df19ab9aed4bae63fa84d5494c898ae8e1221bb'',
		#parentIds : [
			''dacea5333aef4eb02c82e797ce3fc664187cf363'',
			''5df19ab9aed4bae63fa84d5494c898ae8e1221bb''
		]
	},
	HiExampleCommit {
		#id : ''dacea5333aef4eb02c82e797ce3fc664187cf363'',
		#comment : ''remove (at least for now) setting for system repositories since we are not using them any longer '',
		#parentIds : [
			''a4cb0fca09f08b655a96ebf150c838315de78357''
		]
	},
	HiExampleCommit {
		#id : ''a4cb0fca09f08b655a96ebf150c838315de78357'',
		#comment : ''fix checkout remote branchs'',
		#parentIds : [
			''521cc515ff6af585064d25a3a666ed92b4b506b9''
		]
	},
	HiExampleCommit {
		#id : ''5df19ab9aed4bae63fa84d5494c898ae8e1221bb'',
		#comment : ''Merge solving rename to IceTipCommitDiffModel'',
		#parentIds : [
			''0ceddccfbdce5533ae889218410a871cec2b6bd6''
		]
	},
	HiExampleCommit {
		#id : ''0ceddccfbdce5533ae889218410a871cec2b6bd6'',
		#comment : ''Merge 521cc515ff6af585064d25a3a666ed92b4b506b9'',
		#parentIds : [
			''b1581eb1554205a3cbcf10e08609a79000c74f41'',
			''521cc515ff6af585064d25a3a666ed92b4b506b9''
		]
	},
	HiExampleCommit {
		#id : ''b1581eb1554205a3cbcf10e08609a79000c74f41'',
		#comment : ''mergeBase -> commonAncestorWith\nAdd TipMergePreview\nAdd common superclass between TipMerge and TipCheckout preview'',
		#parentIds : [
			''36bfdb4f184886e73969d385696a08d1afa75127''
		]
	},
	HiExampleCommit {
		#id : ''521cc515ff6af585064d25a3a666ed92b4b506b9'',
		#comment : ''restore wrong merge'',
		#parentIds : [
			''37eed1d7f4f46bc26d9cf0b4d85a3a43a6428bef''
		]
	},
	HiExampleCommit {
		#id : ''37eed1d7f4f46bc26d9cf0b4d85a3a43a6428bef'',
		#comment : ''add missing method'',
		#parentIds : [
			''bebbbfbd04739b964f9cffab5f5b74994ab76eb4''
		]
	},
	HiExampleCommit {
		#id : ''bebbbfbd04739b964f9cffab5f5b74994ab76eb4'',
		#comment : ''refactor standard actions to accept repositories (so we can use it for error recovering, etc.)'',
		#parentIds : [
			''e5ce62e9682bfe3aef388f34a9efff652797e155''
		]
	},
	HiExampleCommit {
		#id : ''e5ce62e9682bfe3aef388f34a9efff652797e155'',
		#comment : ''fix revert for adition and modifications: it needs to visit children too.'',
		#parentIds : [
			''5c2ed3be977d1d6a0102b0a33cde36473f29426a''
		]
	},
	HiExampleCommit {
		#id : ''5c2ed3be977d1d6a0102b0a33cde36473f29426a'',
		#comment : ''Merge 36bfdb4f184886e73969d385696a08d1afa75127'',
		#parentIds : [
			''4b8227d30ff18503c099fc2aa94f34e272855fcc'',
			''36bfdb4f184886e73969d385696a08d1afa75127''
		]
	},
	HiExampleCommit {
		#id : ''4b8227d30ff18503c099fc2aa94f34e272855fcc'',
		#comment : ''fix some minor bugs'',
		#parentIds : [
			''ff003f083e42f9c780262a18358f7aa368e8411a''
		]
	},
	HiExampleCommit {
		#id : ''ff003f083e42f9c780262a18358f7aa368e8411a'',
		#comment : ''renamed'',
		#parentIds : [
			''0fa44e7ba98377e96abb14ab64591085ce5e61d2''
		]
	},
	HiExampleCommit {
		#id : ''0fa44e7ba98377e96abb14ab64591085ce5e61d2'',
		#comment : ''modelling the diffFromHead and diffToHead difference on how we need to present things'',
		#parentIds : [
			''2eb63dc93321d21d359baf4e9d330d3764f7fe3d''
		]
	},
	HiExampleCommit {
		#id : ''36bfdb4f184886e73969d385696a08d1afa75127'',
		#comment : ''Add discard and load preview'',
		#parentIds : [
			''3cb1ced03f11c3f8e2e350ea4e8813fe19d38791''
		]
	},
	HiExampleCommit {
		#id : ''3cb1ced03f11c3f8e2e350ea4e8813fe19d38791'',
		#comment : ''Cleanups\n- renames unmarkDirty -> markAsClean\n'',
		#parentIds : [
			''fdfc004e092fa02fe7cc3f8576c612915a40d061''
		]
	},
	HiExampleCommit {
		#id : ''fdfc004e092fa02fe7cc3f8576c612915a40d061'',
		#comment : ''Cleanups\n - rename checkout -> chechoutAllPackages\n - rename checkout* -> load* since they only load without changing the reference commit\n - make tests work'',
		#parentIds : [
			''10a145ba79255b9067a6ec74c1bc7a4d073f7925''
		]
	},
	HiExampleCommit {
		#id : ''10a145ba79255b9067a6ec74c1bc7a4d073f7925'',
		#comment : ''Fix commit metaclass changes'',
		#parentIds : [
			''56954cd613baad713cb8df43788be4ae70bb13bf''
		]
	},
	HiExampleCommit {
		#id : ''56954cd613baad713cb8df43788be4ae70bb13bf'',
		#comment : ''Added IceTipCheckoutModel to manage checkout and checkout strategies'',
		#parentIds : [
			''519a86801d51608e035ae9758bb9c8805cfbd73f''
		]
	},
	HiExampleCommit {
		#id : ''519a86801d51608e035ae9758bb9c8805cfbd73f'',
		#comment : ''Mark dirty packages when adopting a commit\n- Also added iceberg initialization through plugin'',
		#parentIds : [
			''2daf51a8e9eb30895464130f0031e4775c57910f''
		]
	},
	HiExampleCommit {
		#id : ''2daf51a8e9eb30895464130f0031e4775c57910f'',
		#comment : ''Merge branch \''monticello-decoupling\'' into dev-0.7\n'',
		#parentIds : [
			''2eb63dc93321d21d359baf4e9d330d3764f7fe3d'',
			''3a17917974c6963eda0c56742e7ed486a4a5c148''
		]
	},
	HiExampleCommit {
		#id : ''2eb63dc93321d21d359baf4e9d330d3764f7fe3d'',
		#comment : ''enhance comparison for method definitions'',
		#parentIds : [
			''3854c11c2d26b4db5721ab8df123aa8293700ae0''
		]
	},
	HiExampleCommit {
		#id : ''3854c11c2d26b4db5721ab8df123aa8293700ae0'',
		#comment : ''add "add remote" command (still empty)'',
		#parentIds : [
			''d8e3e7289025c6e9c80e5bc57fafd7e46a1e2dba''
		]
	},
	HiExampleCommit {
		#id : ''d8e3e7289025c6e9c80e5bc57fafd7e46a1e2dba'',
		#comment : ''put "add repository" at right because I\''m putting all "add" stuff at right'',
		#parentIds : [
			''54a42eb31d0018a30ebc9c8a269dcb3df4423689''
		]
	},
	HiExampleCommit {
		#id : ''54a42eb31d0018a30ebc9c8a269dcb3df4423689'',
		#comment : ''opening new UI by default'',
		#parentIds : [
			''941420b6e02c20a84353405dfe7c153424656797''
		]
	},
	HiExampleCommit {
		#id : ''941420b6e02c20a84353405dfe7c153424656797'',
		#comment : ''using  a standard action for fetching'',
		#parentIds : [
			''568b8d62d588c0553172dde2353525af414661cd''
		]
	},
	HiExampleCommit {
		#id : ''568b8d62d588c0553172dde2353525af414661cd'',
		#comment : ''add default merge type.'',
		#parentIds : [
			''231338c61139d0c618f7dd50d417975d41b2b810''
		]
	},
	HiExampleCommit {
		#id : ''231338c61139d0c618f7dd50d417975d41b2b810'',
		#comment : ''- add clone again strategy to repair repositories\n- enhance UI feedback of cloning operations\n- enhance visual feedback of clone dialogs (append user and project to location)'',
		#parentIds : [
			''ac8000a6fa182af2f6ae093130d86752a84ea4e7''
		]
	},
	HiExampleCommit {
		#id : ''ac8000a6fa182af2f6ae093130d86752a84ea4e7'',
		#comment : ''using an (arguably) better approach than use #isKindOf:'',
		#parentIds : [
			''8d227167ce99668572fd666e9c204d458a59bcf4''
		]
	},
	HiExampleCommit {
		#id : ''8d227167ce99668572fd666e9c204d458a59bcf4'',
		#comment : ''enhance some comments and change how some are wrote (because talkign yoda good is not)'',
		#parentIds : [
			''9fe00aa4d2086c775c916f6f24a05a16f331bc45''
		]
	},
	HiExampleCommit {
		#id : ''9fe00aa4d2086c775c916f6f24a05a16f331bc45'',
		#comment : ''being smarter with the selection of branches that I can remove (just locals)'',
		#parentIds : [
			''fd29429b2bf443f9006d81c216202aee81cd3ecb''
		]
	},
	HiExampleCommit {
		#id : ''fd29429b2bf443f9006d81c216202aee81cd3ecb'',
		#comment : ''add some comments'',
		#parentIds : [
			''c1a7b306b171da2bdbae19ada3c92868585e5e40''
		]
	},
	HiExampleCommit {
		#id : ''c1a7b306b171da2bdbae19ada3c92868585e5e40'',
		#comment : ''minor text adjustments'',
		#parentIds : [
			''60a1f68df4fc3967b705c5b63293a7737dc417ed''
		]
	},
	HiExampleCommit {
		#id : ''60a1f68df4fc3967b705c5b63293a7737dc417ed'',
		#comment : ''add strategies to checkout process (all and loaded for now, "baseline" will come with metacello plugin)\n '',
		#parentIds : [
			''48c01eeb9d8d567ce49e448d2856a1f3c659c47c''
		]
	},
	HiExampleCommit {
		#id : ''48c01eeb9d8d567ce49e448d2856a1f3c659c47c'',
		#comment : ''fix checkout of remote branchs'',
		#parentIds : [
			''d752f32389cd6f3977cc044221ca4d94dc3a5e9f''
		]
	},
	HiExampleCommit {
		#id : ''d752f32389cd6f3977cc044221ca4d94dc3a5e9f'',
		#comment : ''raising "IceNotInBranch" exception instear a generic'',
		#parentIds : [
			''6eee44bfd479fbd76c70ea1bccd798a3c1cc76bc''
		]
	},
	HiExampleCommit {
		#id : ''6eee44bfd479fbd76c70ea1bccd798a3c1cc76bc'',
		#comment : ''- "fix" the repository outline so  it can receive different kind of operations.\n- add remove remote option.\n- dix a problem accesing branchs when in detached mode (because they do not have any sense)'',
		#parentIds : [
			''b5b6457988d742a1a60ce79c7a78a0f56e21bbb4''
		]
	},
	HiExampleCommit {
		#id : ''3a17917974c6963eda0c56742e7ed486a4a5c148'',
		#comment : ''Remove unused methods'',
		#parentIds : [
			''e7e604b4336b5c6755de7daddf25a9ead09ca7b6''
		]
	},
	HiExampleCommit {
		#id : ''e7e604b4336b5c6755de7daddf25a9ead09ca7b6'',
		#comment : ''Restoring metacello load$'',
		#parentIds : [
			''09c87f4be3737f7460805edca48cdfffa07daacc''
		]
	},
	HiExampleCommit {
		#id : ''09c87f4be3737f7460805edca48cdfffa07daacc'',
		#comment : ''Cleaning working copy update methods'',
		#parentIds : [
			''c1adc77ee634c2b7ab012b2ff62802508c4b45ea''
		]
	},
	HiExampleCommit {
		#id : ''c1adc77ee634c2b7ab012b2ff62802508c4b45ea'',
		#comment : ''Simplify working copy initialization\n - reference commit points to head at repository creation time\n - simplify package dictionary initialization'',
		#parentIds : [
			''1dc9c88131628618c0619b6e2b8df85297494986''
		]
	},
	HiExampleCommit {
		#id : ''1dc9c88131628618c0619b6e2b8df85297494986'',
		#comment : ''Removed unused #fetchFinished'',
		#parentIds : [
			''696107d804686197b378f702320a45b4f5119514''
		]
	},
	HiExampleCommit {
		#id : ''696107d804686197b378f702320a45b4f5119514'',
		#comment : ''Cleaning monticello dependencies$'',
		#parentIds : [
			''a23611442777c03ae74914c6a0015826057dcb4e''
		]
	},
	HiExampleCommit {
		#id : ''a23611442777c03ae74914c6a0015826057dcb4e'',
		#comment : ''Not loaded packages cannot be dirty'',
		#parentIds : [
			''6bcc5a9e2fa941079dc10fa72b6e801b1f16f420''
		]
	},
	HiExampleCommit {
		#id : ''6bcc5a9e2fa941079dc10fa72b6e801b1f16f420'',
		#comment : ''Cloned repository should be set in a location according to the project'',
		#parentIds : [
			''b01c7d05e9939c0e3a5c97dc94be13eb0a6c6a0c''
		]
	},
	HiExampleCommit {
		#id : ''b01c7d05e9939c0e3a5c97dc94be13eb0a6c6a0c'',
		#comment : ''Cleaning tests using package states'',
		#parentIds : [
			''b8dcbd66a744690e8af9bc1b2927f9545ec76548''
		]
	},
	HiExampleCommit {
		#id : ''b8dcbd66a744690e8af9bc1b2927f9545ec76548'',
		#comment : ''Refresh repositories on external change'',
		#parentIds : [
			''20c8718445121c4e17e5fd7aad3e858d61612182''
		]
	},
	HiExampleCommit {
		#id : ''20c8718445121c4e17e5fd7aad3e858d61612182'',
		#comment : ''Remove package states'',
		#parentIds : [
			''096da738d8ad9c98fbc09dce2f9db902b0c4e016''
		]
	},
	HiExampleCommit {
		#id : ''096da738d8ad9c98fbc09dce2f9db902b0c4e016'',
		#comment : ''Do not use package states anymore'',
		#parentIds : [
			''57b212fe96794101a33510ce5596bc0bd711066f''
		]
	},
	HiExampleCommit {
		#id : ''57b212fe96794101a33510ce5596bc0bd711066f'',
		#comment : ''isModified => isDirty'',
		#parentIds : [
			''25b4646994013b776525aa47ef2178689f5aea8f''
		]
	},
	HiExampleCommit {
		#id : ''25b4646994013b776525aa47ef2178689f5aea8f'',
		#comment : ''Package dirtyness is managed in iceberg'',
		#parentIds : [
			''0468b37237c5837751ad356078d008c1501de003''
		]
	},
	HiExampleCommit {
		#id : ''b5b6457988d742a1a60ce79c7a78a0f56e21bbb4'',
		#comment : ''- add repair "locate repository" otption\n- fix a bug when editing repository location (we need to reset repository handle)'',
		#parentIds : [
			''0d76e58f2195b9410a9e24d3549b36f3aa70e089''
		]
	},
	HiExampleCommit {
		#id : ''0d76e58f2195b9410a9e24d3549b36f3aa70e089'',
		#comment : ''implement "discard changes and load" repair strategy'',
		#parentIds : [
			''1c47675f1789084ad9d561ebd520c7f448996353''
		]
	},
	HiExampleCommit {
		#id : ''1c47675f1789084ad9d561ebd520c7f448996353'',
		#comment : ''remove "labels" from commit info (still not implemented and is not urgent)'',
		#parentIds : [
			''6ed7a6fe50242b9d0a2edf3bf6b61f6af942c813''
		]
	},
	HiExampleCommit {
		#id : ''6ed7a6fe50242b9d0a2edf3bf6b61f6af942c813'',
		#comment : ''Merge 0468b37237c5837751ad356078d008c1501de003'',
		#parentIds : [
			''0ba3a68f7491878df5a2a9e13550a6d2c2d15f68'',
			''0468b37237c5837751ad356078d008c1501de003''
		]
	},
	HiExampleCommit {
		#id : ''0468b37237c5837751ad356078d008c1501de003'',
		#comment : ''Add Inspect action everywhere'',
		#parentIds : [
			''dbe7c184723ac3b3767303517ab916f3b9adeb3b''
		]
	},
	HiExampleCommit {
		#id : ''0ba3a68f7491878df5a2a9e13550a6d2c2d15f68'',
		#comment : ''add fetch button to pull browser$'',
		#parentIds : [
			''bac937425b176bcc3ea88e2237c353c23d8100c5''
		]
	},
	HiExampleCommit {
		#id : ''bac937425b176bcc3ea88e2237c353c23d8100c5'',
		#comment : ''close merge and branch windows after operation is done'',
		#parentIds : [
			''984816e6506b7fa53cc82f69f286812f75d6bc7f''
		]
	},
	HiExampleCommit {
		#id : ''984816e6506b7fa53cc82f69f286812f75d6bc7f'',
		#comment : ''close commit if there are no more changes to commit'',
		#parentIds : [
			''66db623b0f9724ef58c6ee2f4e512fe29860607e''
		]
	},
	HiExampleCommit {
		#id : ''66db623b0f9724ef58c6ee2f4e512fe29860607e'',
		#comment : ''pull/push window needs to close after executed (otherwise is very annoying ;) )'',
		#parentIds : [
			''0f03679b6b3ade749686c9c01b191477b33f5c5f''
		]
	},
	HiExampleCommit {
		#id : ''0f03679b6b3ade749686c9c01b191477b33f5c5f'',
		#comment : ''Merge dbe7c184723ac3b3767303517ab916f3b9adeb3b'',
		#parentIds : [
			''ef6b8c2ce7d52a2d4c79e81531af7ecd33455fdf'',
			''dbe7c184723ac3b3767303517ab916f3b9adeb3b''
		]
	},
	HiExampleCommit {
		#id : ''dbe7c184723ac3b3767303517ab916f3b9adeb3b'',
		#comment : ''Fix recover strategy:\n - should merge both reference and branch commit to have the desired effect'',
		#parentIds : [
			''ab19bf2518ad1fd08e714235d149bd1a6287e915''
		]
	},
	HiExampleCommit {
		#id : ''ef6b8c2ce7d52a2d4c79e81531af7ecd33455fdf'',
		#comment : ''badge color and text color moved to themes (so we can adapt themes)'',
		#parentIds : [
			''2cb717f3cabd7068779a262081a85a49e16e8984''
		]
	},
	HiExampleCommit {
		#id : ''2cb717f3cabd7068779a262081a85a49e16e8984'',
		#comment : ''this needs to be changed$'',
		#parentIds : [
			''ad397d87c3af816d09d7b474ce18b6c2de57760e''
		]
	},
	HiExampleCommit {
		#id : ''9026538b34fd6d343011f83ecf895e471050fec1'',
		#comment : ''Merge pull request #586 from seandenigris/patch-1\n\nCreate git_cookbook.md'',
		#parentIds : [
			''c806f568ef9f8be077e049f483be279403a4e067'',
			''f3b3439eab82d4691b7a4a641be7029522d4a8cc''
		]
	},
	HiExampleCommit {
		#id : ''f3b3439eab82d4691b7a4a641be7029522d4a8cc'',
		#comment : ''Create git_cookbook.md'',
		#parentIds : [
			''c806f568ef9f8be077e049f483be279403a4e067''
		]
	},
	HiExampleCommit {
		#id : ''ab19bf2518ad1fd08e714235d149bd1a6287e915'',
		#comment : ''Mark packages as clean after discarding changes'',
		#parentIds : [
			''bb357db693729d78cfd3a49eed183169979aa5d2''
		]
	},
	HiExampleCommit {
		#id : ''bb357db693729d78cfd3a49eed183169979aa5d2'',
		#comment : ''Reload all should be discard changes'',
		#parentIds : [
			''a64aa2702ea39762b1ce5b1d00e8850eb0be3c2e''
		]
	},
	HiExampleCommit {
		#id : ''ad397d87c3af816d09d7b474ce18b6c2de57760e'',
		#comment : ''add repair checkout option'',
		#parentIds : [
			''a64aa2702ea39762b1ce5b1d00e8850eb0be3c2e''
		]
	},
	HiExampleCommit {
		#id : ''a64aa2702ea39762b1ce5b1d00e8850eb0be3c2e'',
		#comment : ''- implement merge with branch repair option\n- ignoring directories because git does not supports them (and they will be created with non-directory changes)'',
		#parentIds : [
			''b6ef4eb6e4e89b6d526b691fe3c9fbfb20823793''
		]
	},
	HiExampleCommit {
		#id : ''b6ef4eb6e4e89b6d526b691fe3c9fbfb20823793'',
		#comment : ''ignoring directories because git does not supports them (and they will be created with non-directory changes)'',
		#parentIds : [
			''b8099c9fc7b86b11000fb2d69f53425ec4bcef0f''
		]
	},
	HiExampleCommit {
		#id : ''b8099c9fc7b86b11000fb2d69f53425ec4bcef0f'',
		#comment : ''implement merge with branch repair option'',
		#parentIds : [
			''4bb4cd0eb219ac54be2b7503ec2488007544a856''
		]
	},
	HiExampleCommit {
		#id : ''4bb4cd0eb219ac54be2b7503ec2488007544a856'',
		#comment : ''implement repair merging option\nsome refactor'',
		#parentIds : [
			''cbb52d7be994efb2744265dbfd3df15ed3def0e5''
		]
	},
	HiExampleCommit {
		#id : ''cbb52d7be994efb2744265dbfd3df15ed3def0e5'',
		#comment : ''using correct checkout new branch method$'',
		#parentIds : [
			''f1754dc54a3c425cffae363c3a94c15b34a7e614''
		]
	},
	HiExampleCommit {
		#id : ''f1754dc54a3c425cffae363c3a94c15b34a7e614'',
		#comment : ''work on repair tool'',
		#parentIds : [
			''f278c70a3182f9b5a391df40b5212679f3297754''
		]
	},
	HiExampleCommit {
		#id : ''f278c70a3182f9b5a391df40b5212679f3297754'',
		#comment : ''- refactor toolbar and statusbar to extract common behaviour. \n- add left/right capabilities to the statusbar\n- add branch info on status bar'',
		#parentIds : [
			''1121676dcc626d49322754f30eafc6e22f03ed50''
		]
	},
	HiExampleCommit {
		#id : ''1121676dcc626d49322754f30eafc6e22f03ed50'',
		#comment : ''simplified usage of actions, I create a "Standard" action who will apply to most cases, while preserving the capability of create custom actions in case of need.'',
		#parentIds : [
			''86530aa0ee7a806b42c66c22a9d844de5cfcdaef''
		]
	},
	HiExampleCommit {
		#id : ''86530aa0ee7a806b42c66c22a9d844de5cfcdaef'',
		#comment : ''recategorisation$'',
		#parentIds : [
			''44fe1aa10e56eebc01ff4946bf4ab9a273f7f161''
		]
	},
	HiExampleCommit {
		#id : ''44fe1aa10e56eebc01ff4946bf4ab9a273f7f161'',
		#comment : ''fetch should not be possible if there are no remotes defined'',
		#parentIds : [
			''73b81cb39d9d856d83a84b9e4b92d8078d830e5b''
		]
	},
	HiExampleCommit {
		#id : ''73b81cb39d9d856d83a84b9e4b92d8078d830e5b'',
		#comment : ''- delete branch needs to go in an action with UI feedback \n- created a generic UI action that will handle the visual feedback, etc'',
		#parentIds : [
			''0324f5278ee7fd29d3b416c83e7a8b17ebdce5fc''
		]
	},
	HiExampleCommit {
		#id : ''0324f5278ee7fd29d3b416c83e7a8b17ebdce5fc'',
		#comment : ''renamed'',
		#parentIds : [
			''cc6186f8750c60d64417c299134ee06b660c0660''
		]
	},
	HiExampleCommit {
		#id : ''cc6186f8750c60d64417c299134ee06b660c0660'',
		#comment : ''add packages command implemented$'',
		#parentIds : [
			''a24bf617530f9a23176a88508af7c71e2eb7b258''
		]
	},
	HiExampleCommit {
		#id : ''a24bf617530f9a23176a88508af7c71e2eb7b258'',
		#comment : ''Add delete branch command\n - In model: extend git branches to provide delete operation, \n - In UI model: add new commander command, extended IceTipBranchModel\n - IceTipRepositoryBrowser should create context with selection\n - IceTipRepositoryBrowser should refresh branch list\n - IceTipRepositoryContext should include selected item\n - IceTipTreeDataSource should provide model and not just #data'',
		#parentIds : [
			''645202b3d639c13c440351f809b0f82e24c34ef7''
		]
	},
	HiExampleCommit {
		#id : ''645202b3d639c13c440351f809b0f82e24c34ef7'',
		#comment : ''Tree entry has an object that is a blob'',
		#parentIds : [
			''b587f4d46c2b5f0b0b59be7250a32cc48c5801a1''
		]
	},
	HiExampleCommit {
		#id : ''b587f4d46c2b5f0b0b59be7250a32cc48c5801a1'',
		#comment : ''extract reload all to an action'',
		#parentIds : [
			''f077b564d1a157ac7b2987663614b2f01b17ef67''
		]
	},
	HiExampleCommit {
		#id : ''f077b564d1a157ac7b2987663614b2f01b17ef67'',
		#comment : ''preview checkout/merge now shows diff (not the commit list, etc.)'',
		#parentIds : [
			''50940b9c6d694d3eede0ad88f33424d51e22fade''
		]
	},
	HiExampleCommit {
		#id : ''50940b9c6d694d3eede0ad88f33424d51e22fade'',
		#comment : ''Merge 2769c9b81a63e7fc988b32c4b0922e745371c36c'',
		#parentIds : [
			''816eb5d9eaeaba7f81dea9ffe86b26e9a391d06a'',
			''2769c9b81a63e7fc988b32c4b0922e745371c36c''
		]
	},
	HiExampleCommit {
		#id : ''816eb5d9eaeaba7f81dea9ffe86b26e9a391d06a'',
		#comment : ''make initialization resilient when there is no default push remote defined. '',
		#parentIds : [
			''81c6fa100a079fa3a3c665705f521b9a2d438e71''
		]
	},
	HiExampleCommit {
		#id : ''81c6fa100a079fa3a3c665705f521b9a2d438e71'',
		#comment : ''fix revert (still not refreshing) and remove "Redo". I will need to create an "install" command.$'',
		#parentIds : [
			''54af4c3294f6e50d1468119bb76b9055bb6ee099''
		]
	},
	HiExampleCommit {
		#id : ''54af4c3294f6e50d1468119bb76b9055bb6ee099'',
		#comment : ''fix a problem when drawing tables with proportional layout.'',
		#parentIds : [
			''3d73ddad52bb79e83eb74312288e4791752a05a0''
		]
	},
	HiExampleCommit {
		#id : ''2769c9b81a63e7fc988b32c4b0922e745371c36c'',
		#comment : ''Fix writing of package definitions'',
		#parentIds : [
			''911f17c4256b980cb60ba7b834910298e7a27ac6''
		]
	},
	HiExampleCommit {
		#id : ''3d73ddad52bb79e83eb74312288e4791752a05a0'',
		#comment : ''renamed to something clearer'',
		#parentIds : [
			''911f17c4256b980cb60ba7b834910298e7a27ac6''
		]
	},
	HiExampleCommit {
		#id : ''911f17c4256b980cb60ba7b834910298e7a27ac6'',
		#comment : ''comments$'',
		#parentIds : [
			''036cc4168abc848411cf0a9e4ad08866698b3ec8''
		]
	},
	HiExampleCommit {
		#id : ''036cc4168abc848411cf0a9e4ad08866698b3ec8'',
		#comment : ''merge panel using the same metaphor as checkout'',
		#parentIds : [
			''862d16a3291b0dd992bf21c30d4139296d0db014''
		]
	},
	HiExampleCommit {
		#id : ''862d16a3291b0dd992bf21c30d4139296d0db014'',
		#comment : ''comments$'',
		#parentIds : [
			''6274a0bdd09001138c3ad8933f6accecb57f6e7e''
		]
	},
	HiExampleCommit {
		#id : ''6274a0bdd09001138c3ad8933f6accecb57f6e7e'',
		#comment : ''fix pull/push to get also repository model'',
		#parentIds : [
			''0bf199f782aa35e13aede21b119dad650ff99e2b''
		]
	},
	HiExampleCommit {
		#id : ''0bf199f782aa35e13aede21b119dad650ff99e2b'',
		#comment : ''using an easier style of "checkout branch". I guess I need to replicate that on the merge dialog too.'',
		#parentIds : [
			''51c5af2b520aac400c043acf8b71eee13b0ae89a''
		]
	},
	HiExampleCommit {
		#id : ''51c5af2b520aac400c043acf8b71eee13b0ae89a'',
		#comment : ''add a first version of merge working'',
		#parentIds : [
			''d67b359bd89fd478b8f279479404adf1b967e43d''
		]
	},
	HiExampleCommit {
		#id : ''d67b359bd89fd478b8f279479404adf1b967e43d'',
		#comment : ''refactor to extract selection of branch (to reuse it for merging)'',
		#parentIds : [
			''321ffa70279b8322b75a11c9260ff0d103b360ec''
		]
	},
	HiExampleCommit {
		#id : ''321ffa70279b8322b75a11c9260ff0d103b360ec'',
		#comment : ''- reorder merge button\n- rename methods to fit new name'',
		#parentIds : [
			''b80584428125b4efa418be611596ff869ece81b2''
		]
	},
	HiExampleCommit {
		#id : ''b80584428125b4efa418be611596ff869ece81b2'',
		#comment : ''using my own fast table for options because I can have better control. \n(maybe is time to extract that into Spec?)'',
		#parentIds : [
			''fcf32a77438264f9185a15c894d69112d66472e8''
		]
	},
	HiExampleCommit {
		#id : ''fcf32a77438264f9185a15c894d69112d66472e8'',
		#comment : ''add inspect commit to inspect window'',
		#parentIds : [
			''302eaad85840ebe721ea1c97f63ee99b91a881b9''
		]
	},
	HiExampleCommit {
		#id : ''302eaad85840ebe721ea1c97f63ee99b91a881b9'',
		#comment : ''more renames (and add a context specific for History browser)'',
		#parentIds : [
			''15ec4e15764c50c5d3f38b385ffda50ee538a895''
		]
	},
	HiExampleCommit {
		#id : ''15ec4e15764c50c5d3f38b385ffda50ee538a895'',
		#comment : ''generalise filters to be used where they are needed'',
		#parentIds : [
			''c86750eba92d3d83428f859235a5231865a3ebfd''
		]
	},
	HiExampleCommit {
		#id : ''c86750eba92d3d83428f859235a5231865a3ebfd'',
		#comment : ''- add repositoryModel to those we need it.\n- refactor some names\n'',
		#parentIds : [
			''3a8c1525d530d83b80dc04b8309b80952438556e''
		]
	},
	HiExampleCommit {
		#id : ''3a8c1525d530d83b80dc04b8309b80952438556e'',
		#comment : ''new fix (maybe it works this time :P)'',
		#parentIds : [
			''422a28235cb1ca401789c24432485fe46d344cc0''
		]
	},
	HiExampleCommit {
		#id : ''422a28235cb1ca401789c24432485fe46d344cc0'',
		#comment : ''fix refresh of action browsers'',
		#parentIds : [
			''7756fcbea6a938008df3d2e1d6b0c45ff8409978''
		]
	},
	HiExampleCommit {
		#id : ''7756fcbea6a938008df3d2e1d6b0c45ff8409978'',
		#comment : ''validate branches do not have spaces'',
		#parentIds : [
			''1b5fc65aec9070ee6958cb84c1616ee538ee986d''
		]
	},
	HiExampleCommit {
		#id : ''1b5fc65aec9070ee6958cb84c1616ee538ee986d'',
		#comment : ''fix branch model creation (they have to have repositoryModel and in general, beCached)'',
		#parentIds : [
			''9187acc1fd9827d8d09a3293809f59f82869fde7''
		]
	},
	HiExampleCommit {
		#id : ''9187acc1fd9827d8d09a3293809f59f82869fde7'',
		#comment : ''renamed'',
		#parentIds : [
			''32491c0662c50c56b87c45bc6d40101061a2dfe3''
		]
	},
	HiExampleCommit {
		#id : ''32491c0662c50c56b87c45bc6d40101061a2dfe3'',
		#comment : ''build of toolbar needs to happen on #rebuildToolbar. Otherwise items are destroyed and not replaced.'',
		#parentIds : [
			''a94103b427d8f55e89297c68726824980dae5368''
		]
	},
	HiExampleCommit {
		#id : ''a94103b427d8f55e89297c68726824980dae5368'',
		#comment : ''if there is no pull remote, we need to skip the fetch'',
		#parentIds : [
			''6d1babe7af52c73e3aedae5fbbd06e83f351e49b''
		]
	},
	HiExampleCommit {
		#id : ''6d1babe7af52c73e3aedae5fbbd06e83f351e49b'',
		#comment : ''toolbar initialization happens on #initializeToolbar (like that we can cancel it on children if needed)'',
		#parentIds : [
			''c0ebc1e121cff18acff21592cde3132755767d2a''
		]
	},
	HiExampleCommit {
		#id : ''c0ebc1e121cff18acff21592cde3132755767d2a'',
		#comment : ''Checkout working as expected. But still lots of checks to do.'',
		#parentIds : [
			''fcb4791e39d0f6c42f9bffe46d1195a348eae1e6''
		]
	},
	HiExampleCommit {
		#id : ''fcb4791e39d0f6c42f9bffe46d1195a348eae1e6'',
		#comment : ''comments$'',
		#parentIds : [
			''7cb4bc9026b58ce6188a8e1628eb4143e1d696b8''
		]
	},
	HiExampleCommit {
		#id : ''7cb4bc9026b58ce6188a8e1628eb4143e1d696b8'',
		#comment : ''fix some datasources without reference to the owner tool (we need it later for build menus and stuff)'',
		#parentIds : [
			''57c05a3e8ca9395f27396a61b21b0356d7fa3521''
		]
	},
	HiExampleCommit {
		#id : ''57c05a3e8ca9395f27396a61b21b0356d7fa3521'',
		#comment : ''fix selection (when there are no selections :P)'',
		#parentIds : [
			''f34c803f73006a67529e01f852049e3d208a2580''
		]
	},
	HiExampleCommit {
		#id : ''f34c803f73006a67529e01f852049e3d208a2580'',
		#comment : ''add unload package'',
		#parentIds : [
			''59478956425d58471cc66252c75ec19d849cfb09''
		]
	},
	HiExampleCommit {
		#id : ''59478956425d58471cc66252c75ec19d849cfb09'',
		#comment : ''better (?) titles'',
		#parentIds : [
			''71412deb42b7cb78d721a3397bf3a1bb0fe30078''
		]
	},
	HiExampleCommit {
		#id : ''71412deb42b7cb78d721a3397bf3a1bb0fe30078'',
		#comment : ''fix browse package and add some <noCache> pragmas to make the cache mechanism to skip actions.'',
		#parentIds : [
			''890207d25179e0c9193f8e60ec3ef24396398588''
		]
	},
	HiExampleCommit {
		#id : ''890207d25179e0c9193f8e60ec3ef24396398588'',
		#comment : ''- checkout branch dialog (backend still WIP) \n- also add a stub for merge dialog\n- lots of refactor'',
		#parentIds : [
			''9018770e26f97f39fbd24939cefa0e3ce47e0fc5''
		]
	},
	HiExampleCommit {
		#id : ''9018770e26f97f39fbd24939cefa0e3ce47e0fc5'',
		#comment : ''Reuse the repository handle'',
		#parentIds : [
			''55e3fb98985b8cc15a669853707b81e19d28decb''
		]
	},
	HiExampleCommit {
		#id : ''55e3fb98985b8cc15a669853707b81e19d28decb'',
		#comment : ''Avoid multiple calls to workingCopyState$'',
		#parentIds : [
			''ec62d4cacb9abd40fd12067eca8407639162796d''
		]
	},
	HiExampleCommit {
		#id : ''ec62d4cacb9abd40fd12067eca8407639162796d'',
		#comment : ''Restore commit cache'',
		#parentIds : [
			''81e67794fc730913020db1d990aea6bcd4db5524''
		]
	},
	HiExampleCommit {
		#id : ''81e67794fc730913020db1d990aea6bcd4db5524'',
		#comment : ''Moving lib git repository settings to lib git repository$'',
		#parentIds : [
			''e39741f4362e3b8a379d1b1739edb17921f394db''
		]
	},
	HiExampleCommit {
		#id : ''e39741f4362e3b8a379d1b1739edb17921f394db'',
		#comment : ''Using LibGitRepository'',
		#parentIds : [
			''3c303ecead6efdb69ce49c95966a98932983b37c''
		]
	},
	HiExampleCommit {
		#id : ''3c303ecead6efdb69ce49c95966a98932983b37c'',
		#comment : ''Cleanup Iceberg Settings\n - move settings to Libgit backend\n - cleaning class side critiques'',
		#parentIds : [
			''0f9ccf33e3a08b90501847c6d70d490133517b25''
		]
	},
	HiExampleCommit {
		#id : ''0f9ccf33e3a08b90501847c6d70d490133517b25'',
		#comment : ''Merge 3e252edc0b48985da3eea0aed79bca5fedd1ed3f'',
		#parentIds : [
			''a3cabdf47bd6a222e1bf13bf1259b1741bb3cdbc'',
			''3e252edc0b48985da3eea0aed79bca5fedd1ed3f''
		]
	},
	HiExampleCommit {
		#id : ''a3cabdf47bd6a222e1bf13bf1259b1741bb3cdbc'',
		#comment : ''Add properties file to index on repository creation'',
		#parentIds : [
			''d15952d55498956309f54e89248cd7ab9439fb6d''
		]
	},
	HiExampleCommit {
		#id : ''3e252edc0b48985da3eea0aed79bca5fedd1ed3f'',
		#comment : ''add missing method'',
		#parentIds : [
			''a1097262d0d49e79b1362cc341c5332d6f17e8cb''
		]
	},
	HiExampleCommit {
		#id : ''a1097262d0d49e79b1362cc341c5332d6f17e8cb'',
		#comment : ''Merge fb37992139f53aaede70193ea7178ff0b1c4d0a8'',
		#parentIds : [
			''475d9daddd00f8351e7da13e4004c32490dd8d6f'',
			''fb37992139f53aaede70193ea7178ff0b1c4d0a8''
		]
	},
	HiExampleCommit {
		#id : ''d15952d55498956309f54e89248cd7ab9439fb6d'',
		#comment : ''Upgraded Index as property of a repository'',
		#parentIds : [
			''fb37992139f53aaede70193ea7178ff0b1c4d0a8''
		]
	},
	HiExampleCommit {
		#id : ''475d9daddd00f8351e7da13e4004c32490dd8d6f'',
		#comment : ''add some functionality to the tree filter: \n- it focus on editor when you start write, \n- but you can jump back to the list with arrow up and down (and it will put you in different places of the list: begining or end)\n'',
		#parentIds : [
			''35a0efe0abc19fd23c2e35b74860aac4bde58e29''
		]
	},
	HiExampleCommit {
		#id : ''35a0efe0abc19fd23c2e35b74860aac4bde58e29'',
		#comment : ''add IceTipTreeLinkFilterFunction to link a filter with a text presenter.$'',
		#parentIds : [
			''edafb2c26c742c59dcd4071f91a104ab7ce3bf9c''
		]
	},
	HiExampleCommit {
		#id : ''fb37992139f53aaede70193ea7178ff0b1c4d0a8'',
		#comment : ''Cleanups and comments'',
		#parentIds : [
			''51f4ee25c7c901aff21ab8d2611e2763ce23eceb''
		]
	},
	HiExampleCommit {
		#id : ''51f4ee25c7c901aff21ab8d2611e2763ce23eceb'',
		#comment : ''Update Libgit version'',
		#parentIds : [
			''7452c31c545d55254fac95e91a26705cd027a6fa''
		]
	},
	HiExampleCommit {
		#id : ''7452c31c545d55254fac95e91a26705cd027a6fa'',
		#comment : ''Fix old UI'',
		#parentIds : [
			''3e105ae7c8bbb2f91e2ae9ea539a08f92a4955bf''
		]
	},
	HiExampleCommit {
		#id : ''edafb2c26c742c59dcd4071f91a104ab7ce3bf9c'',
		#comment : ''Merge 3e105ae7c8bbb2f91e2ae9ea539a08f92a4955bf'',
		#parentIds : [
			''329ff18b51eb3f77a00329da81407eaabf20e910'',
			''3e105ae7c8bbb2f91e2ae9ea539a08f92a4955bf''
		]
	},
	HiExampleCommit {
		#id : ''3e105ae7c8bbb2f91e2ae9ea539a08f92a4955bf'',
		#comment : ''Remove unused IceTreeDiff class'',
		#parentIds : [
			''32c798de618b968bd1fa25293acb3c8af9c451ce''
		]
	},
	HiExampleCommit {
		#id : ''32c798de618b968bd1fa25293acb3c8af9c451ce'',
		#comment : ''Comments and protocols'',
		#parentIds : [
			''03d9e0f48101e66900a9cc3e08d126cef3279012''
		]
	},
	HiExampleCommit {
		#id : ''329ff18b51eb3f77a00329da81407eaabf20e910'',
		#comment : ''first version of IceTipCheckoutBranchPanel (WIP)#'',
		#parentIds : [
			''03d9e0f48101e66900a9cc3e08d126cef3279012''
		]
	},
	HiExampleCommit {
		#id : ''03d9e0f48101e66900a9cc3e08d126cef3279012'',
		#comment : ''Support modification of package categories'',
		#parentIds : [
			''90390297c5b51ced7eb2c1e22048f172078da909''
		]
	},
	HiExampleCommit {
		#id : ''90390297c5b51ced7eb2c1e22048f172078da909'',
		#comment : ''Add support to commit wide strings'',
		#parentIds : [
			''27603ed49d6db063e030684123b56f4ffb290098''
		]
	},
	HiExampleCommit {
		#id : ''27603ed49d6db063e030684123b56f4ffb290098'',
		#comment : ''Merge db728b8360064f4ca08730e59731eb809d6e0ca8'',
		#parentIds : [
			''0c6598f7a2bb15c9993a25e72013bcfe5d623123'',
			''db728b8360064f4ca08730e59731eb809d6e0ca8''
		]
	},
	HiExampleCommit {
		#id : ''0c6598f7a2bb15c9993a25e72013bcfe5d623123'',
		#comment : ''Fix broken IceInteractiveErrorVisitor creation'',
		#parentIds : [
			''76ab948918db968671f9b3216c0d6fbac5614768''
		]
	},
	HiExampleCommit {
		#id : ''76ab948918db968671f9b3216c0d6fbac5614768'',
		#comment : ''Cleanups in Iceberg-Changes\n - comments\n - method categorization\n - unused method removals'',
		#parentIds : [
			''857c97b692ff86889d595b8aea563262f585be35''
		]
	},
	HiExampleCommit {
		#id : ''db728b8360064f4ca08730e59731eb809d6e0ca8'',
		#comment : ''try to make status bar to align better. Failing for now, but changes are needed.'',
		#parentIds : [
			''9a85ff0d0d135df6e40c5843b17782f4e6b28bea''
		]
	},
	HiExampleCommit {
		#id : ''9a85ff0d0d135df6e40c5843b17782f4e6b28bea'',
		#comment : ''missing method'',
		#parentIds : [
			''ca6baa27cd4fadf3445845216cda664d3cacd0da''
		]
	},
	HiExampleCommit {
		#id : ''ca6baa27cd4fadf3445845216cda664d3cacd0da'',
		#comment : ''create a model structure for trees to better handle branches by group.'',
		#parentIds : [
			''551e7140b8ccce5ed6e157e16cd4c6c08190f628''
		]
	},
	HiExampleCommit {
		#id : ''551e7140b8ccce5ed6e157e16cd4c6c08190f628'',
		#comment : ''push up methods I was using far too much :)'',
		#parentIds : [
			''ef42f8676b4fdc8c802e61c3103ebaa75764daf0''
		]
	},
	HiExampleCommit {
		#id : ''ef42f8676b4fdc8c802e61c3103ebaa75764daf0'',
		#comment : ''add comment'',
		#parentIds : [
			''0bc81ee1378e347c9ad764436f3e939084806594''
		]
	},
	HiExampleCommit {
		#id : ''0bc81ee1378e347c9ad764436f3e939084806594'',
		#comment : ''better name'',
		#parentIds : [
			''857c97b692ff86889d595b8aea563262f585be35''
		]
	},
	HiExampleCommit {
		#id : ''857c97b692ff86889d595b8aea563262f585be35'',
		#comment : ''restore refresh lost code'',
		#parentIds : [
			''8602110e22cc0da4c01715645e5da45ae59c887c''
		]
	},
	HiExampleCommit {
		#id : ''8602110e22cc0da4c01715645e5da45ae59c887c'',
		#comment : ''first version of a statusBar'',
		#parentIds : [
			''92cc5676fb063fa2685df7a4a523f34196808f4f''
		]
	},
	HiExampleCommit {
		#id : ''92cc5676fb063fa2685df7a4a523f34196808f4f'',
		#comment : ''refactor name'',
		#parentIds : [
			''1b5d2ea3ee43cd5829a2156db899c464217aac21''
		]
	},
	HiExampleCommit {
		#id : ''1b5d2ea3ee43cd5829a2156db899c464217aac21'',
		#comment : ''refactor and ensuring all commands receive all needed parameters  '',
		#parentIds : [
			''f89442e54c3e1239afb56d1b0e66adc47634a4ec''
		]
	},
	HiExampleCommit {
		#id : ''f89442e54c3e1239afb56d1b0e66adc47634a4ec'',
		#comment : ''selecting available commands, instead build with all.'',
		#parentIds : [
			''de3118449096e570f2e7fb515bf09e80d3805f9e''
		]
	},
	HiExampleCommit {
		#id : ''de3118449096e570f2e7fb515bf09e80d3805f9e'',
		#comment : ''use new api of commander'',
		#parentIds : [
			''a92bd668502a986b3979cd99fba7349eafc75906''
		]
	},
	HiExampleCommit {
		#id : ''a92bd668502a986b3979cd99fba7349eafc75906'',
		#comment : ''commit/pull/push just can be done if repository is attached'',
		#parentIds : [
			''2491ae1c573a4d4eb11fae38a84748b4028b0f48''
		]
	},
	HiExampleCommit {
		#id : ''2491ae1c573a4d4eb11fae38a84748b4028b0f48'',
		#comment : ''comments'',
		#parentIds : [
			''e0f4bb6b4c94ac85ef05951aa4a041e1dd33baa5''
		]
	},
	HiExampleCommit {
		#id : ''e0f4bb6b4c94ac85ef05951aa4a041e1dd33baa5'',
		#comment : ''comments'',
		#parentIds : [
			''238b84dbc18eea5bb261d2f3ddd1407c4c894a28''
		]
	},
	HiExampleCommit {
		#id : ''238b84dbc18eea5bb261d2f3ddd1407c4c894a28'',
		#comment : ''switched order because is better to have first the commit list.'',
		#parentIds : [
			''2564e347322367d9f293511255846b5caa6a5df5''
		]
	},
	HiExampleCommit {
		#id : ''2564e347322367d9f293511255846b5caa6a5df5'',
		#comment : ''add shortcut to browse (and tweaked a bit  names, etc.)'',
		#parentIds : [
			''560dc1179bb318d3587cd3bfef45d63639d386f9''
		]
	},
	HiExampleCommit {
		#id : ''560dc1179bb318d3587cd3bfef45d63639d386f9'',
		#comment : ''cmd+s to work on comment panel too'',
		#parentIds : [
			''726db1313bb40f06502b29ebe7f47b76f5057446''
		]
	},
	HiExampleCommit {
		#id : ''726db1313bb40f06502b29ebe7f47b76f5057446'',
		#comment : ''I think is better to start on commit list and not in the remote list'',
		#parentIds : [
			''3bb81ae891375c5af5b8709c18be263bacc26265''
		]
	},
	HiExampleCommit {
		#id : ''3bb81ae891375c5af5b8709c18be263bacc26265'',
		#comment : ''add comments'',
		#parentIds : [
			''c6d0779f7093353b1a583a1299df9ce8ce13aadb''
		]
	},
	HiExampleCommit {
		#id : ''c6d0779f7093353b1a583a1299df9ce8ce13aadb'',
		#comment : ''add a special version of fast table that can act as an outline: \n\n- roots are not collapsable\n- outline component is transparent'',
		#parentIds : [
			''bf95377b88fef402bfe7ec24e3b8cb996dce1b57''
		]
	},
	HiExampleCommit {
		#id : ''bf95377b88fef402bfe7ec24e3b8cb996dce1b57'',
		#comment : ''better sice for action browsers'',
		#parentIds : [
			''1bfea8a7ee4b6d308e347d3a6a79737871e49530''
		]
	},
	HiExampleCommit {
		#id : ''1bfea8a7ee4b6d308e347d3a6a79737871e49530'',
		#comment : ''refactors and comments '',
		#parentIds : [
			''be1c63f1f94893baa51c8a0c3056a3bd38ce2f84''
		]
	},
	HiExampleCommit {
		#id : ''be1c63f1f94893baa51c8a0c3056a3bd38ce2f84'',
		#comment : ''Merge d17ad835ffe50ac5ec890ee1c3b9f2715a7c1d53'',
		#parentIds : [
			''e5c199d3d53faa5613620e78268877837b891d0f'',
			''d17ad835ffe50ac5ec890ee1c3b9f2715a7c1d53''
		]
	},
	HiExampleCommit {
		#id : ''e5c199d3d53faa5613620e78268877837b891d0f'',
		#comment : ''suspend annoucements when reloading all because it takes infinite time otherwise.'',
		#parentIds : [
			''af21fb480d6d2df2d47230d02ca91d28e7677cf5''
		]
	},
	HiExampleCommit {
		#id : ''af21fb480d6d2df2d47230d02ca91d28e7677cf5'',
		#comment : ''fixed a problem creating merge tree\n'',
		#parentIds : [
			''8e109b82bdaefafda916a338d735188239d4741c''
		]
	},
	HiExampleCommit {
		#id : ''d17ad835ffe50ac5ec890ee1c3b9f2715a7c1d53'',
		#comment : ''- Iceberg should depend on latest commander to load\n- add class trait importing'',
		#parentIds : [
			''2951281622483c11df937adc76f066b37612b939''
		]
	},
	HiExampleCommit {
		#id : ''8e109b82bdaefafda916a338d735188239d4741c'',
		#comment : ''implement missing visitClassTraitDefinition:'',
		#parentIds : [
			''ed8a21a7da79887787402db0776f17dc8661bd57''
		]
	},
	HiExampleCommit {
		#id : ''ed8a21a7da79887787402db0776f17dc8661bd57'',
		#comment : ''make sidebar transpartent (looks better, but now I still need to disable the square when focused)'',
		#parentIds : [
			''2951281622483c11df937adc76f066b37612b939''
		]
	},
	HiExampleCommit {
		#id : ''2951281622483c11df937adc76f066b37612b939'',
		#comment : ''sending diff will perform the diff, as it was up to now  it was happening twice'',
		#parentIds : [
			''bd2cf73ec6d3aa36c8536e34f5d28d25f9a285dd''
		]
	},
	HiExampleCommit {
		#id : ''bd2cf73ec6d3aa36c8536e34f5d28d25f9a285dd'',
		#comment : ''Show also  class comments when diffing.'',
		#parentIds : [
			''3d26ebbad576875c7b43d66c2c0746b4b3e91dff''
		]
	},
	HiExampleCommit {
		#id : ''3d26ebbad576875c7b43d66c2c0746b4b3e91dff'',
		#comment : ''add badges also for modeIcon'',
		#parentIds : [
			''07983ab74b0cd4d5e4d3378fd84f492a789478ed''
		]
	},
	HiExampleCommit {
		#id : ''07983ab74b0cd4d5e4d3378fd84f492a789478ed'',
		#comment : ''add comments'',
		#parentIds : [
			''cab392ee16f0baedc5b1a6dc6781054da46ae7a3''
		]
	},
	HiExampleCommit {
		#id : ''cab392ee16f0baedc5b1a6dc6781054da46ae7a3'',
		#comment : ''first version of badge working :)'',
		#parentIds : [
			''3afc9213ea08ea1eb0ed73e4a28480150906fe24''
		]
	},
	HiExampleCommit {
		#id : ''3afc9213ea08ea1eb0ed73e4a28480150906fe24'',
		#comment : ''Merge fdadc03beb9f2305fea6661a0e02f1ca891ef217'',
		#parentIds : [
			''2d9e8c764210aa78a528c68d951f70186070caa6'',
			''fdadc03beb9f2305fea6661a0e02f1ca891ef217''
		]
	},
	HiExampleCommit {
		#id : ''2d9e8c764210aa78a528c68d951f70186070caa6'',
		#comment : ''adding generic cmd+s action for "dialog-like" browsers (commit, pull and push). \nthe only problem is that it does not works for commit because rubric does not let me use cmd+s unless I suspend autoAccept.\n'',
		#parentIds : [
			''e689430933bc026854256dffe2df179f8700fb3b''
		]
	},
	HiExampleCommit {
		#id : ''fdadc03beb9f2305fea6661a0e02f1ca891ef217'',
		#comment : ''Including Iceberg-TipUI as a dependency, so it is loaded in the baseline\n'',
		#parentIds : [
			''9196c408316633dee7937d3f9764cac0bfb1799d''
		]
	},
	HiExampleCommit {
		#id : ''9196c408316633dee7937d3f9764cac0bfb1799d'',
		#comment : ''Updating Commander dependency to use the latest one in Pharo 7\n'',
		#parentIds : [
			''6b24fb442d597f6b10d545ddc959cd8ef14a34b9''
		]
	},
	HiExampleCommit {
		#id : ''e689430933bc026854256dffe2df179f8700fb3b'',
		#comment : ''not really fixed. Now I\''m getting properly the text.'',
		#parentIds : [
			''61a3a7d6cd0cccf6c311aee32d808063662b31a6''
		]
	},
	HiExampleCommit {
		#id : ''61a3a7d6cd0cccf6c311aee32d808063662b31a6'',
		#comment : ''kind of fixed accept action (trigger commit when accepting text)'',
		#parentIds : [
			''764df07217002c47c488481617a8945f694f0c8e''
		]
	},
	HiExampleCommit {
		#id : ''764df07217002c47c488481617a8945f694f0c8e'',
		#comment : ''validating when commit does not has a message (not this case, but yes the previous one who was commited by mistake)'',
		#parentIds : [
			''66a22aa322e8b4b6d4cd2c76c13459cba435469d''
		]
	},
	HiExampleCommit {
		#id : ''66a22aa322e8b4b6d4cd2c76c13459cba435469d'',
		#comment : '''',
		#parentIds : [
			''6c0b5bf282b74af0d87a53bc5371f22b285b4727''
		]
	},
	HiExampleCommit {
		#id : ''6c0b5bf282b74af0d87a53bc5371f22b285b4727'',
		#comment : ''cleaning correctly commit panel once commit is done.'',
		#parentIds : [
			''6028f842986ad15ee982f5ddacbc5f66e6957567''
		]
	},
	HiExampleCommit {
		#id : ''6028f842986ad15ee982f5ddacbc5f66e6957567'',
		#comment : ''better comment'',
		#parentIds : [
			''137dce8e8a4bec7a29e8de5989e7f71326453100''
		]
	},
	HiExampleCommit {
		#id : ''137dce8e8a4bec7a29e8de5989e7f71326453100'',
		#comment : ''transform  MCPackageModified announcements into IceRepositoryModified announcements.'',
		#parentIds : [
			''e3b42fe62508ae2ebd759b9367abd4843aba2c87''
		]
	},
	HiExampleCommit {
		#id : ''e3b42fe62508ae2ebd759b9367abd4843aba2c87'',
		#comment : ''fix an error when selecting remotes.'',
		#parentIds : [
			''cadd3a77a3db5ab257492813c676c1a881a8383d''
		]
	},
	HiExampleCommit {
		#id : ''cadd3a77a3db5ab257492813c676c1a881a8383d'',
		#comment : ''rename'',
		#parentIds : [
			''2ea97c9c33b4ae857f79ff3393956c6a2d9e47f8''
		]
	},
	HiExampleCommit {
		#id : ''2ea97c9c33b4ae857f79ff3393956c6a2d9e47f8'',
		#comment : ''fix show correctly first commit (without ancestors).\nBEWARE: This fix requires IceMemoryChange to be present on image. I think that needs to be refactored out of Iceberg-Memory package.'',
		#parentIds : [
			''6b24fb442d597f6b10d545ddc959cd8ef14a34b9''
		]
	},
	HiExampleCommit {
		#id : ''6b24fb442d597f6b10d545ddc959cd8ef14a34b9'',
		#comment : ''fix packages->packageModels '',
		#parentIds : [
			''5b8a600e1c815b8833bbe89962cf1c3195116e8d''
		]
	},
	HiExampleCommit {
		#id : ''5b8a600e1c815b8833bbe89962cf1c3195116e8d'',
		#comment : ''repository browser now lists correctly versions'',
		#parentIds : [
			''7d0fcfbbc752b936d874fd481af8ed055ab2bafb''
		]
	},
	HiExampleCommit {
		#id : ''7d0fcfbbc752b936d874fd481af8ed055ab2bafb'',
		#comment : ''log window needs to show old left/new right.'',
		#parentIds : [
			''8493253a96471c23f6eb7ac4776397b454c1814d''
		]
	},
	HiExampleCommit {
		#id : ''8493253a96471c23f6eb7ac4776397b454c1814d'',
		#comment : ''fix refresh problem (and an incorrect text)'',
		#parentIds : [
			''0ad949222f7653f0e2137b9e66b9d3e0d79521f9''
		]
	},
	HiExampleCommit {
		#id : ''0ad949222f7653f0e2137b9e66b9d3e0d79521f9'',
		#comment : ''Refactor: Actions needs to be handled as commands. '',
		#parentIds : [
			''bf466c897e73149a46ca9ecda35dac6389cdbdd3''
		]
	},
	HiExampleCommit {
		#id : ''bf466c897e73149a46ca9ecda35dac6389cdbdd3'',
		#comment : ''browse loaded packages just if they are loaded :)'',
		#parentIds : [
			''5dbbc44909cf45361f263bc4a90dd1b852b85846''
		]
	},
	HiExampleCommit {
		#id : ''5dbbc44909cf45361f263bc4a90dd1b852b85846'',
		#comment : ''allow browse of unloaded packages'',
		#parentIds : [
			''f141634da25d38362ba372c794e3eebbfbf3b077''
		]
	},
	HiExampleCommit {
		#id : ''f141634da25d38362ba372c794e3eebbfbf3b077'',
		#comment : ''fix attempt to selection if there is no current selection.'',
		#parentIds : [
			''13e848594172a181b3ca731560057610ac13ed70''
		]
	},
	HiExampleCommit {
		#id : ''13e848594172a181b3ca731560057610ac13ed70'',
		#comment : ''implement browse and browse repository version'',
		#parentIds : [
			''c85f858682fdc11604d8007cdb5bcd870e413b9e''
		]
	},
	HiExampleCommit {
		#id : ''c85f858682fdc11604d8007cdb5bcd870e413b9e'',
		#comment : ''double click now opens package browser (seems better)'',
		#parentIds : [
			''e9f08e12b876e8d221589c2331771491826d019c''
		]
	},
	HiExampleCommit {
		#id : ''e9f08e12b876e8d221589c2331771491826d019c'',
		#comment : ''implemented browse package command\n '',
		#parentIds : [
			''aadd92a2cbde13b10786296194c70e2824913d40''
		]
	},
	HiExampleCommit {
		#id : ''aadd92a2cbde13b10786296194c70e2824913d40'',
		#comment : ''descriptionText and statusText renamed to descriptionWithDecoration and statusWithDecoration'',
		#parentIds : [
			''12f8ba24c87bfff960fd768c994d9edf2271927b''
		]
	},
	HiExampleCommit {
		#id : ''12f8ba24c87bfff960fd768c994d9edf2271927b'',
		#comment : ''- options in package now appear if they can be applied.\n- implemented "load" function'',
		#parentIds : [
			''f241097cfc97a05d9e3ad87c63c0d9051ba7a4fb''
		]
	},
	HiExampleCommit {
		#id : ''f241097cfc97a05d9e3ad87c63c0d9051ba7a4fb'',
		#comment : ''fix initialization of browser (it was initialization always pull models)'',
		#parentIds : [
			''3b7071b80a6debe3ee94a628852ec1f79154190e''
		]
	},
	HiExampleCommit {
		#id : ''3b7071b80a6debe3ee94a628852ec1f79154190e'',
		#comment : ''fix labels'',
		#parentIds : [
			''b485926834c98adb7453af640d0553801a6fb5bc''
		]
	},
	HiExampleCommit {
		#id : ''b485926834c98adb7453af640d0553801a6fb5bc'',
		#comment : ''add remote destination to push/pull and refactors'',
		#parentIds : [
			''e48d459c4e33eaa0b1093262c47c4c9585c249a8''
		]
	},
	HiExampleCommit {
		#id : ''e48d459c4e33eaa0b1093262c47c4c9585c249a8'',
		#comment : ''fix'',
		#parentIds : [
			''714accca79b88c88c0e1812a7b2826a89ef86150''
		]
	},
	HiExampleCommit {
		#id : ''714accca79b88c88c0e1812a7b2826a89ef86150'',
		#comment : ''Recover Esteban\''s changes'',
		#parentIds : [
			''fd25c0f762e34e1436ff18d67526118b75fcad1f''
		]
	},
	HiExampleCommit {
		#id : ''fd25c0f762e34e1436ff18d67526118b75fcad1f'',
		#comment : ''Add install action'',
		#parentIds : [
			''b426442814f13bbf9511f68145d16d38bcfd1a1b''
		]
	},
	HiExampleCommit {
		#id : ''b426442814f13bbf9511f68145d16d38bcfd1a1b'',
		#comment : ''Update disk working copy with changing HEAD branch'',
		#parentIds : [
			''4c44950f76932d3a140a2730937910344387bf2b''
		]
	},
	HiExampleCommit {
		#id : ''4c44950f76932d3a140a2730937910344387bf2b'',
		#comment : ''Support better missing ocal repositories\n - calculate package status without the need for the HEAD commit\n - Simplify IceDetachedPackage and IceSynchronizedPackage into SingleCommitPackage\n - redirected UI descriptions'',
		#parentIds : [
			''5bdd593e09323416bed21a02cf7eb840dd499e80''
		]
	},
	HiExampleCommit {
		#id : ''5bdd593e09323416bed21a02cf7eb840dd499e80'',
		#comment : ''Optimize Diff calculation\n - ask #writerClass only once\n - rebuild mc package from image only if package is dirty\n\nCleanup: Remove IceGitDiff'',
		#parentIds : [
			''e7d5c73290781a622f14ea143f1a8d857875268f''
		]
	},
	HiExampleCommit {
		#id : ''e7d5c73290781a622f14ea143f1a8d857875268f'',
		#comment : ''Make synchronization view work on detached head state'',
		#parentIds : [
			''0a555e668639941184435e55d6d969b3ce4927d0''
		]
	},
	HiExampleCommit {
		#id : ''0a555e668639941184435e55d6d969b3ce4927d0'',
		#comment : ''Rename Status to RepositoryStatus'',
		#parentIds : [
			''ff93948418f94a230182063bef0fce0bd06802bc''
		]
	},
	HiExampleCommit {
		#id : ''ff93948418f94a230182063bef0fce0bd06802bc'',
		#comment : ''- Move #description to UI package\n- Add browse methods to DiffNode\n- Reverting no modifications'',
		#parentIds : [
			''21d007907f257260d4fe09af8738f9f4e7c115fb''
		]
	},
	HiExampleCommit {
		#id : ''21d007907f257260d4fe09af8738f9f4e7c115fb'',
		#comment : ''Add tests for merge and inverse merge'',
		#parentIds : [
			''018c86d24d5ad9d3b9f78af2d9f9d2b53f33993a''
		]
	},
	HiExampleCommit {
		#id : ''018c86d24d5ad9d3b9f78af2d9f9d2b53f33993a'',
		#comment : ''Add fast forward and merge into image case'',
		#parentIds : [
			''c08e074fd7a87eb5a849094f7a4355bddf9c91c5''
		]
	},
	HiExampleCommit {
		#id : ''c08e074fd7a87eb5a849094f7a4355bddf9c91c5'',
		#comment : ''Adding advance in image test'',
		#parentIds : [
			''5377ccc4cd950d014ffd1cb1e90f1bc4ba5b22f2''
		]
	},
	HiExampleCommit {
		#id : ''5377ccc4cd950d014ffd1cb1e90f1bc4ba5b22f2'',
		#comment : ''Testing first four merge cases'',
		#parentIds : [
			''10ecbbae8050e0428fdc9abc7d05ccd364f3262f''
		]
	},
	HiExampleCommit {
		#id : ''10ecbbae8050e0428fdc9abc7d05ccd364f3262f'',
		#comment : ''add a first version of a pull browser'',
		#parentIds : [
			''737b5c0b5ae1f4600b67b7f2b35276e45c125e55''
		]
	},
	HiExampleCommit {
		#id : ''737b5c0b5ae1f4600b67b7f2b35276e45c125e55'',
		#comment : ''if no item is selected, I don\''t need to do anything (or I will have a DNU)'',
		#parentIds : [
			''98e6a7df1bacbe601b6a55c06196ab7e936732e1''
		]
	},
	HiExampleCommit {
		#id : ''98e6a7df1bacbe601b6a55c06196ab7e936732e1'',
		#comment : ''enable shortcuts on diff  panel'',
		#parentIds : [
			''28b73f6cc99600784eb0dbc99d7e9555e0ddd07b''
		]
	},
	HiExampleCommit {
		#id : ''28b73f6cc99600784eb0dbc99d7e9555e0ddd07b'',
		#comment : ''commits needs to swap sides (at least for now, until I can see what happens)'',
		#parentIds : [
			''c0b5cab0c9b65cb54532b4b1659d56284fd7e61f''
		]
	},
	HiExampleCommit {
		#id : ''c0b5cab0c9b65cb54532b4b1659d56284fd7e61f'',
		#comment : ''diff panel can swap sides (we need to be sure that old/new comes always in same order.'',
		#parentIds : [
			''d8ee0404ce64dd4c597169236677d7c1ee13d705''
		]
	},
	HiExampleCommit {
		#id : ''d8ee0404ce64dd4c597169236677d7c1ee13d705'',
		#comment : ''add context menus on diff panels (implement mechanism for tree datasources)'',
		#parentIds : [
			''55a5b1e79f2ed3f9ceabe9740b73bc7648e2f735''
		]
	},
	HiExampleCommit {
		#id : ''55a5b1e79f2ed3f9ceabe9740b73bc7648e2f735'',
		#comment : ''better look for push browser'',
		#parentIds : [
			''7b7e19688c5a54afc346610886c1340f291ab87c''
		]
	},
	HiExampleCommit {
		#id : ''7b7e19688c5a54afc346610886c1340f291ab87c'',
		#comment : ''fix selection/deselection of commits (and show tabs) DNU problem.'',
		#parentIds : [
			''feae12cc4ae9345ed773a8d78031886d0fef7453''
		]
	},
	HiExampleCommit {
		#id : ''feae12cc4ae9345ed773a8d78031886d0fef7453'',
		#comment : ''fix refresh problem'',
		#parentIds : [
			''eba5b4b8dba8dd29ebd81a2e7f04ece0854a9f25''
		]
	},
	HiExampleCommit {
		#id : ''eba5b4b8dba8dd29ebd81a2e7f04ece0854a9f25'',
		#comment : ''refactor some trees to add an "outline" tree'',
		#parentIds : [
			''d337539d9600a5dd5251d72f540dea51164c9202''
		]
	},
	HiExampleCommit {
		#id : ''d337539d9600a5dd5251d72f540dea51164c9202'',
		#comment : ''add a push browser/implement push action'',
		#parentIds : [
			''23ea6c86fd9840ff0ed35defec66035775d06a8a''
		]
	},
	HiExampleCommit {
		#id : ''23ea6c86fd9840ff0ed35defec66035775d06a8a'',
		#comment : ''inform user a commit is happening'',
		#parentIds : [
			''e9b2525eff2dbeb6ff9382798eb834f4675dea2f''
		]
	},
	HiExampleCommit {
		#id : ''e9b2525eff2dbeb6ff9382798eb834f4675dea2f'',
		#comment : ''- swapp panels (but I guess what we have to swap is the test)\n- move refresh up'',
		#parentIds : [
			''dee4bfdfb05285f34cd439caa149072e4901dc93''
		]
	},
	HiExampleCommit {
		#id : ''dee4bfdfb05285f34cd439caa149072e4901dc93'',
		#comment : ''force reset contents (otherwise they remain there)'',
		#parentIds : [
			''8b1aa2c7ad262c07d77eadadc20189bdfa215b6e''
		]
	},
	HiExampleCommit {
		#id : ''8b1aa2c7ad262c07d77eadadc20189bdfa215b6e'',
		#comment : ''will force an update (testing)'',
		#parentIds : [
			''ee90d190b45d8ca8fceaa6c1b4376329304d917a''
		]
	},
	HiExampleCommit {
		#id : ''ee90d190b45d8ca8fceaa6c1b4376329304d917a'',
		#comment : ''clean diff panel if no selection'',
		#parentIds : [
			''c6a909275c67d373d46c7550a69c2b702a6d6cd2''
		]
	},
	HiExampleCommit {
		#id : ''c6a909275c67d373d46c7550a69c2b702a6d6cd2'',
		#comment : ''inform a commit happened an change needs to be shown'',
		#parentIds : [
			''ad729f3547abff3ea92a838d76f3bc7954aee08b''
		]
	},
	HiExampleCommit {
		#id : ''ad729f3547abff3ea92a838d76f3bc7954aee08b'',
		#comment : ''workingCopyDiff model should not be cached'',
		#parentIds : [
			''7bc2cae0902f17d432f3dcce2809662a370933c9''
		]
	},
	HiExampleCommit {
		#id : ''7bc2cae0902f17d432f3dcce2809662a370933c9'',
		#comment : ''do not patch working copy if package is not loaded'',
		#parentIds : [
			''14aa5a29d8b3af1d6b191274fdd542729581759e''
		]
	},
	HiExampleCommit {
		#id : ''14aa5a29d8b3af1d6b191274fdd542729581759e'',
		#comment : ''Merge 9896f757dddb3a2f229b244fc2b25c95e403d39b'',
		#parentIds : [
			''8b624b766eb70945fed8bcc2a881f114093bdd5f'',
			''9896f757dddb3a2f229b244fc2b25c95e403d39b''
		]
	},
	HiExampleCommit {
		#id : ''8b624b766eb70945fed8bcc2a881f114093bdd5f'',
		#comment : ''add #resetDiff to clear correctly working copy models'',
		#parentIds : [
			''c4df264d4e02bedc74f48042c6a2dd2d6cb2ecd5''
		]
	},
	HiExampleCommit {
		#id : ''9896f757dddb3a2f229b244fc2b25c95e403d39b'',
		#comment : ''Recovering IceAuthenticationError'',
		#parentIds : [
			''4553c18b66a593c7fb3998358f402f816814c8dd''
		]
	},
	HiExampleCommit {
		#id : ''c4df264d4e02bedc74f48042c6a2dd2d6cb2ecd5'',
		#comment : ''fix commit with new diff'',
		#parentIds : [
			''5d1d9d7e224a8abf97d900625003f5aee9d2449f''
		]
	},
	HiExampleCommit {
		#id : ''5d1d9d7e224a8abf97d900625003f5aee9d2449f'',
		#comment : ''add missing comparisson methods '',
		#parentIds : [
			''c784250e75b0b643ba45ff8f53955a71fec2c164''
		]
	},
	HiExampleCommit {
		#id : ''4553c18b66a593c7fb3998358f402f816814c8dd'',
		#comment : ''Only one broken test'',
		#parentIds : [
			''dd820aee4c9e05b56ce5c6989eff644344806b27''
		]
	},
	HiExampleCommit {
		#id : ''dd820aee4c9e05b56ce5c6989eff644344806b27'',
		#comment : ''Refactoring package dirty marking'',
		#parentIds : [
			''8072d5f1cc5e835160b2704583ac2510aee1f550''
		]
	},
	HiExampleCommit {
		#id : ''8072d5f1cc5e835160b2704583ac2510aee1f550'',
		#comment : ''Fixing many tests'',
		#parentIds : [
			''7b435708716aaf8af18f73cc12818b82299de25f''
		]
	},
	HiExampleCommit {
		#id : ''c784250e75b0b643ba45ff8f53955a71fec2c164'',
		#comment : ''refactor icon names'',
		#parentIds : [
			''7b435708716aaf8af18f73cc12818b82299de25f''
		]
	},
	HiExampleCommit {
		#id : ''7b435708716aaf8af18f73cc12818b82299de25f'',
		#comment : ''Merge df1ef3fdf6c832dedbd014b9c108e38a3dac573e'',
		#parentIds : [
			''ec24fabf01636ee45afca1c32510559717234b26'',
			''df1ef3fdf6c832dedbd014b9c108e38a3dac573e''
		]
	},
	HiExampleCommit {
		#id : ''ec24fabf01636ee45afca1c32510559717234b26'',
		#comment : ''- Extending commit from merge\n- Start fixing tests'',
		#parentIds : [
			''a3e5a652c6497721cbfc5eb66319ab451d5cc29e''
		]
	},
	HiExampleCommit {
		#id : ''df1ef3fdf6c832dedbd014b9c108e38a3dac573e'',
		#comment : ''remove IceAbstractDiff incorrect remaining\n'',
		#parentIds : [
			''3de2bcd3bec7e9392ae8af56272e4b1a53803449''
		]
	},
	HiExampleCommit {
		#id : ''3de2bcd3bec7e9392ae8af56272e4b1a53803449'',
		#comment : ''Merge b0a9f91f8148dcc2cc7f5daea137ebf7c4fda3f0'',
		#parentIds : [
			''a3e5a652c6497721cbfc5eb66319ab451d5cc29e'',
			''b0a9f91f8148dcc2cc7f5daea137ebf7c4fda3f0''
		]
	},
	HiExampleCommit {
		#id : ''a3e5a652c6497721cbfc5eb66319ab451d5cc29e'',
		#comment : ''Remove usages of IceTreeDiff'',
		#parentIds : [
			''129f4277ba15afba1fda594d3a80f3e817bb21db''
		]
	},
	HiExampleCommit {
		#id : ''129f4277ba15afba1fda594d3a80f3e817bb21db'',
		#comment : ''Unmarking dirty packages'',
		#parentIds : [
			''60d00e3b0b3159cfcbf7d19068c61361a72fb122''
		]
	},
	HiExampleCommit {
		#id : ''b0a9f91f8148dcc2cc7f5daea137ebf7c4fda3f0'',
		#comment : ''rename method'',
		#parentIds : [
			''638ad9602e6322b3f02c2bb8a8b3015e022381ff''
		]
	},
	HiExampleCommit {
		#id : ''638ad9602e6322b3f02c2bb8a8b3015e022381ff'',
		#comment : ''(meta-commit)\n- work on commit from IceTip\n- rename some packages\n- UI improvements\n- minor fixes'',
		#parentIds : [
			''0b3cabcd7b0f736c4b57bfafc30840928277df07''
		]
	},
	HiExampleCommit {
		#id : ''60d00e3b0b3159cfcbf7d19068c61361a72fb122'',
		#comment : ''Modified packages should only look at dirtyness'',
		#parentIds : [
			''9368ca9941431f15412788e09268f6d6e19981ce''
		]
	},
	HiExampleCommit {
		#id : ''9368ca9941431f15412788e09268f6d6e19981ce'',
		#comment : ''First try of new merge implementation'',
		#parentIds : [
			''a4151752b544c1e7961250cb5de3a589d9e8ed4f''
		]
	},
	HiExampleCommit {
		#id : ''a4151752b544c1e7961250cb5de3a589d9e8ed4f'',
		#comment : ''Restore cherry pick\n- Cleanup diff and friends'',
		#parentIds : [
			''b1351cb5d9377c64f2f530d3f04981e0138718df''
		]
	},
	HiExampleCommit {
		#id : ''b1351cb5d9377c64f2f530d3f04981e0138718df'',
		#comment : ''Revert modifications'',
		#parentIds : [
			''e583441227005885aec857d7da1c6f1a7703fdb4''
		]
	},
	HiExampleCommit {
		#id : ''e583441227005885aec857d7da1c6f1a7703fdb4'',
		#comment : ''Restore browsing'',
		#parentIds : [
			''df8afbf6641d061c538ec6becec4d1ae699f7051''
		]
	},
	HiExampleCommit {
		#id : ''df8afbf6641d061c538ec6becec4d1ae699f7051'',
		#comment : ''- Repackaging IceErrorVisitor\n- Add IceRevertVisitor'',
		#parentIds : [
			''addf544ceaa5f0f1a10ef3e4b6083bc2e045eac6''
		]
	},
	HiExampleCommit {
		#id : ''0b3cabcd7b0f736c4b57bfafc30840928277df07'',
		#comment : ''clean cached model and allow the existence of <noCache> pragma'',
		#parentIds : [
			''419c1bffeacd6b641db6bc5d741c935adcf2c519''
		]
	},
	HiExampleCommit {
		#id : ''419c1bffeacd6b641db6bc5d741c935adcf2c519'',
		#comment : ''refactor'',
		#parentIds : [
			''9956b6e5cc37e098c2a40f8a7d8b3d1308ae56e0''
		]
	},
	HiExampleCommit {
		#id : ''addf544ceaa5f0f1a10ef3e4b6083bc2e045eac6'',
		#comment : ''Fix bad merge\n'',
		#parentIds : [
			''e44b211e5ff89bd54ead79ff343016bad41b911d''
		]
	},
	HiExampleCommit {
		#id : ''e44b211e5ff89bd54ead79ff343016bad41b911d'',
		#comment : ''Fix bad merge\n'',
		#parentIds : [
			''6e97eb1ac6b55ca6281f030813228fc4e0010c3a''
		]
	},
	HiExampleCommit {
		#id : ''6e97eb1ac6b55ca6281f030813228fc4e0010c3a'',
		#comment : ''Fix bad merge\n'',
		#parentIds : [
			''3dd1365d4f401f3ecd9655fb6881508af5bd45e3''
		]
	},
	HiExampleCommit {
		#id : ''3dd1365d4f401f3ecd9655fb6881508af5bd45e3'',
		#comment : ''Fix bad merge\n'',
		#parentIds : [
			''0f702bc2c66587d9e60f0e27e66f10f83a43714a''
		]
	},
	HiExampleCommit {
		#id : ''0f702bc2c66587d9e60f0e27e66f10f83a43714a'',
		#comment : ''Merge 155436b96e76096bdf1f3cb9aeec13b099f4d95f'',
		#parentIds : [
			''58c7702de56887cbdcac0a0e1de90abe63c62c13'',
			''155436b96e76096bdf1f3cb9aeec13b099f4d95f''
		]
	},
	HiExampleCommit {
		#id : ''58c7702de56887cbdcac0a0e1de90abe63c62c13'',
		#comment : ''Ice conflicting and non conflicting changes'',
		#parentIds : [
			''611dfb41e15a17be56dd16bb58eb69a4fb0c8d50''
		]
	},
	HiExampleCommit {
		#id : ''611dfb41e15a17be56dd16bb58eb69a4fb0c8d50'',
		#comment : ''Recovering IceConflict definition'',
		#parentIds : [
			''75c07fe95ee9a627c6d5b94e946a30dae3fc938c''
		]
	},
	HiExampleCommit {
		#id : ''75c07fe95ee9a627c6d5b94e946a30dae3fc938c'',
		#comment : ''Restore file diff'',
		#parentIds : [
			''7378af9441308c487f7fd7d1deddc57683d1311a''
		]
	},
	HiExampleCommit {
		#id : ''7378af9441308c487f7fd7d1deddc57683d1311a'',
		#comment : ''Remove extra comments'',
		#parentIds : [
			''aea0126723b29192c1a3517e784c504110c9a453''
		]
	},
	HiExampleCommit {
		#id : ''aea0126723b29192c1a3517e784c504110c9a453'',
		#comment : ''Fixing commit after crashed image'',
		#parentIds : [
			''120dba5d8189fa545698eaea1099802c80dc93fd''
		]
	},
	HiExampleCommit {
		#id : ''9956b6e5cc37e098c2a40f8a7d8b3d1308ae56e0'',
		#comment : ''using a command to perform UI actions (just as an idea, seems better?)\nadd some error handling. '',
		#parentIds : [
			''155436b96e76096bdf1f3cb9aeec13b099f4d95f''
		]
	},
	HiExampleCommit {
		#id : ''155436b96e76096bdf1f3cb9aeec13b099f4d95f'',
		#comment : ''add missing reset'',
		#parentIds : [
			''e415f296a6db5a9396d40e20f151748936391366''
		]
	},
	HiExampleCommit {
		#id : ''e415f296a6db5a9396d40e20f151748936391366'',
		#comment : ''refresh when commit'',
		#parentIds : [
			''fe41c3e1260f9a5b83843fd5541ed154788f6116''
		]
	},
	HiExampleCommit {
		#id : ''fe41c3e1260f9a5b83843fd5541ed154788f6116'',
		#comment : ''- working on commit\n- create a diff with selection (to cherry pick) \n- refactors'',
		#parentIds : [
			''a9f513ebf9e58ba2f1132d052b46868ff5ce8358''
		]
	},
	HiExampleCommit {
		#id : ''120dba5d8189fa545698eaea1099802c80dc93fd'',
		#comment : ''Pass on all merge menus'',
		#parentIds : [
			''d480ff1776a6734a9233cc85e592189d3365f2e3''
		]
	},
	HiExampleCommit {
		#id : ''d480ff1776a6734a9233cc85e592189d3365f2e3'',
		#comment : ''Restablished support for fast-forward merge'',
		#parentIds : [
			''2aeb8121320bac89483f2cf96e05c95b02c867e5''
		]
	},
	HiExampleCommit {
		#id : ''2aeb8121320bac89483f2cf96e05c95b02c867e5'',
		#comment : ''Merge menu is almost working'',
		#parentIds : [
			''d285bf184a5ddb532f2215bcd87afe370064eaf1''
		]
	},
	HiExampleCommit {
		#id : ''a9f513ebf9e58ba2f1132d052b46868ff5ce8358'',
		#comment : ''playing with percentages instead fixed values to arrange windows (I liked the "span" function of glamour :( )'',
		#parentIds : [
			''03a35425ba291446fd45a73d0d85049a05954ea7''
		]
	},
	HiExampleCommit {
		#id : ''03a35425ba291446fd45a73d0d85049a05954ea7'',
		#comment : ''working on commit.'',
		#parentIds : [
			''e442a7127c68928dc697895a18d5b6e7e21b3cba''
		]
	},
	HiExampleCommit {
		#id : ''d285bf184a5ddb532f2215bcd87afe370064eaf1'',
		#comment : ''Can browse conflicts'',
		#parentIds : [
			''7c2509ae9b6d83de5b1445448e7c3475e304db8e''
		]
	},
	HiExampleCommit {
		#id : ''e442a7127c68928dc697895a18d5b6e7e21b3cba'',
		#comment : ''first version of a commit panel'',
		#parentIds : [
			''601ca69a25e062f2cca88bb16c739b43f0ee51b4''
		]
	},
	HiExampleCommit {
		#id : ''7c2509ae9b6d83de5b1445448e7c3475e304db8e'',
		#comment : ''Fix history build tree'',
		#parentIds : [
			''62a67699f2750e9031640f8a1af50b740ab5559c''
		]
	},
	HiExampleCommit {
		#id : ''62a67699f2750e9031640f8a1af50b740ab5559c'',
		#comment : ''Creating merge tree with conflicts'',
		#parentIds : [
			''9f4b446079ef22c06e20e68e1cbdb9e5e3c434f9''
		]
	},
	HiExampleCommit {
		#id : ''601ca69a25e062f2cca88bb16c739b43f0ee51b4'',
		#comment : ''fix comments and helps with good tool names (browser, usually)'',
		#parentIds : [
			''3da63bd9ff4db4b11bf757428fbdfa560283aedd''
		]
	},
	HiExampleCommit {
		#id : ''3da63bd9ff4db4b11bf757428fbdfa560283aedd'',
		#comment : ''renamed *Tool to *Browser to make purpose clearer'',
		#parentIds : [
			''db1dbe99298ecdd7496a71fccbcc56695675a969''
		]
	},
	HiExampleCommit {
		#id : ''db1dbe99298ecdd7496a71fccbcc56695675a969'',
		#comment : ''renamed presenters to Panel/Dialog/etc. to make their purpose clearer.'',
		#parentIds : [
			''be0f69b08beb6d816dd5ff1812816b68514b5215''
		]
	},
	HiExampleCommit {
		#id : ''9f4b446079ef22c06e20e68e1cbdb9e5e3c434f9'',
		#comment : ''Removing extra comments'',
		#parentIds : [
			''a677ad2233befffc5b057d6adcb5269b532d253a''
		]
	},
	HiExampleCommit {
		#id : ''a677ad2233befffc5b057d6adcb5269b532d253a'',
		#comment : ''Refactor simmetric difference in merge collect and select operations'',
		#parentIds : [
			''fc03722e057779105fa8bd7e829d005a809d30fe''
		]
	},
	HiExampleCommit {
		#id : ''be0f69b08beb6d816dd5ff1812816b68514b5215'',
		#comment : ''add correct title for repository'',
		#parentIds : [
			''acfa1977016c9358c586ad410d78e7056f9986be''
		]
	},
	HiExampleCommit {
		#id : ''acfa1977016c9358c586ad410d78e7056f9986be'',
		#comment : ''handling contexts and focuses'',
		#parentIds : [
			''d8428f3da8e4c6fa54d0ce00573856cbae4bbb01''
		]
	},
	HiExampleCommit {
		#id : ''fc03722e057779105fa8bd7e829d005a809d30fe'',
		#comment : ''Restore method protocols in diff'',
		#parentIds : [
			''0726a2b08607ddad47f0d540e7a06f268ab1b81e''
		]
	},
	HiExampleCommit {
		#id : ''0726a2b08607ddad47f0d540e7a06f268ab1b81e'',
		#comment : ''See file diff in history'',
		#parentIds : [
			''374ae22472d9fbf026e07bdeb6ac0f7fa32c1c9e''
		]
	},
	HiExampleCommit {
		#id : ''374ae22472d9fbf026e07bdeb6ac0f7fa32c1c9e'',
		#comment : ''Removing Halts'',
		#parentIds : [
			''61b6153a1bf1e56f30ea91e4a466b5e6d7091e07''
		]
	},
	HiExampleCommit {
		#id : ''61b6153a1bf1e56f30ea91e4a466b5e6d7091e07'',
		#comment : ''Missing changes to commit'',
		#parentIds : [
			''160bb5f5253abf2f0db8ba34216b37af966c6ff0''
		]
	},
	HiExampleCommit {
		#id : ''160bb5f5253abf2f0db8ba34216b37af966c6ff0'',
		#comment : ''New changes model'',
		#parentIds : [
			''0ce0d8d9ea157076a20aba4a28fcda608d543ef0''
		]
	},
	HiExampleCommit {
		#id : ''d8428f3da8e4c6fa54d0ce00573856cbae4bbb01'',
		#comment : ''one could override toolbar activation default position '',
		#parentIds : [
			''e3306b4a92d36dae9c95a52cb8d29a8982eb8804''
		]
	},
	HiExampleCommit {
		#id : ''e3306b4a92d36dae9c95a52cb8d29a8982eb8804'',
		#comment : ''obtaining sidebar for IceTipRepositoryTool (WIP)'',
		#parentIds : [
			''33176d6e82c3f9ec7b0581ffc81e5a5c8a91a56b''
		]
	},
	HiExampleCommit {
		#id : ''33176d6e82c3f9ec7b0581ffc81e5a5c8a91a56b'',
		#comment : ''ensure the contexts can receive the appropriate values.'',
		#parentIds : [
			''266e1bd2d00443a69e0f0157652976c012d8b29e''
		]
	},
	HiExampleCommit {
		#id : ''266e1bd2d00443a69e0f0157652976c012d8b29e'',
		#comment : ''refactor'',
		#parentIds : [
			''bf34ba550e120905814f845b7750cf3075636ec3''
		]
	},
	HiExampleCommit {
		#id : ''bf34ba550e120905814f845b7750cf3075636ec3'',
		#comment : ''first version of commit history tool working '',
		#parentIds : [
			''f4b1bb0de244b56826cf22a15c51f048d8f2b99e''
		]
	},
	HiExampleCommit {
		#id : ''f4b1bb0de244b56826cf22a15c51f048d8f2b99e'',
		#comment : ''added activations to repository tool comands'',
		#parentIds : [
			''348ef244fba122ba984c80f90a9bae6fde904abd''
		]
	},
	HiExampleCommit {
		#id : ''348ef244fba122ba984c80f90a9bae6fde904abd'',
		#comment : ''diff panels now are lazy'',
		#parentIds : [
			''0bdf895fa1f9e0c09fedf21f932eacaa0c0398c0''
		]
	},
	HiExampleCommit {
		#id : ''0bdf895fa1f9e0c09fedf21f932eacaa0c0398c0'',
		#comment : ''more work on diffs'',
		#parentIds : [
			''29ab683b0bec1077486284160e3d9bb5f6d4adb2''
		]
	},
	HiExampleCommit {
		#id : ''0ce0d8d9ea157076a20aba4a28fcda608d543ef0'',
		#comment : ''Snapshot of  a non loaded package should not create a mc working copy'',
		#parentIds : [
			''c7774d3c97d9aa7bebdc90ae18e23039009b42da''
		]
	},
	HiExampleCommit {
		#id : ''29ab683b0bec1077486284160e3d9bb5f6d4adb2'',
		#comment : ''working on diff'',
		#parentIds : [
			''0d3f000bb12b51547f3cff7f8a9d82e277e0cce4''
		]
	},
	HiExampleCommit {
		#id : ''0d3f000bb12b51547f3cff7f8a9d82e277e0cce4'',
		#comment : ''add missing method (I forget to properly classify before)'',
		#parentIds : [
			''fc1233f366b4f6ee89f0584e25cdc60b87d3a373''
		]
	},
	HiExampleCommit {
		#id : ''fc1233f366b4f6ee89f0584e25cdc60b87d3a373'',
		#comment : ''work on commitish history tool (to display commit lists) '',
		#parentIds : [
			''fa02668f91bf9b5d1a87fd92ffa7c6cd5effad60''
		]
	},
	HiExampleCommit {
		#id : ''c7774d3c97d9aa7bebdc90ae18e23039009b42da'',
		#comment : ''Handling better extension methods diff'',
		#parentIds : [
			''e6159fa9eb5b7d7a7d5544c37d1beaf31656e4d9''
		]
	},
	HiExampleCommit {
		#id : ''e6159fa9eb5b7d7a7d5544c37d1beaf31656e4d9'',
		#comment : ''Adding file operations'',
		#parentIds : [
			''b2358f3d0eca6e093876a9c5a4f354cb11dbdb1e''
		]
	},
	HiExampleCommit {
		#id : ''fa02668f91bf9b5d1a87fd92ffa7c6cd5effad60'',
		#comment : ''add focus order'',
		#parentIds : [
			''f1a1f2e1a028d8d35cf0c105be97c7ffd9a81433''
		]
	},
	HiExampleCommit {
		#id : ''f1a1f2e1a028d8d35cf0c105be97c7ffd9a81433'',
		#comment : ''fix some names and helps'',
		#parentIds : [
			''fd095fb5d2526ba3f39851cc6018ebd797892789''
		]
	},
	HiExampleCommit {
		#id : ''b2358f3d0eca6e093876a9c5a4f354cb11dbdb1e'',
		#comment : ''Cleanups'',
		#parentIds : [
			''d6b85429ccde4a5b8bdac174a0ed02614a93494b''
		]
	},
	HiExampleCommit {
		#id : ''d6b85429ccde4a5b8bdac174a0ed02614a93494b'',
		#comment : ''Fix commit: should take into account all children recursively when storing.'',
		#parentIds : [
			''061efc6eb0a39bef1aa29fa8f702709293cb7f5b''
		]
	},
	HiExampleCommit {
		#id : ''061efc6eb0a39bef1aa29fa8f702709293cb7f5b'',
		#comment : ''Extract change model'',
		#parentIds : [
			''41c8bc00aadd5e72f5a589e40f1595fcd81ebfec''
		]
	},
	HiExampleCommit {
		#id : ''fd095fb5d2526ba3f39851cc6018ebd797892789'',
		#comment : ''add TipUI and Commander dependency to baseline'',
		#parentIds : [
			''9571276a883842d967b70ec4e3a07082344e35fe''
		]
	},
	HiExampleCommit {
		#id : ''9571276a883842d967b70ec4e3a07082344e35fe'',
		#comment : ''add shortcut support'',
		#parentIds : [
			''86f54c105d394432d01777e30974f509f5604a4b''
		]
	},
	HiExampleCommit {
		#id : ''c806f568ef9f8be077e049f483be279403a4e067'',
		#comment : ''update instructions with 0.6.8'',
		#parentIds : [
			''0a781c949148084b2bb4d12f650220492d989be5''
		]
	},
	HiExampleCommit {
		#id : ''41c8bc00aadd5e72f5a589e40f1595fcd81ebfec'',
		#comment : ''Modifying diff model supporting files and directories'',
		#parentIds : [
			''e158d78dc59ba05ef1c1a5b14a2a1c9e0595784a''
		]
	},
	HiExampleCommit {
		#id : ''86f54c105d394432d01777e30974f509f5604a4b'',
		#comment : ''fixes'',
		#parentIds : [
			''093c9d6a346a580dfe9b43348db3b295439e60bc''
		]
	},
	HiExampleCommit {
		#id : ''093c9d6a346a580dfe9b43348db3b295439e60bc'',
		#comment : ''ensure toolbar looks fine with all themes'',
		#parentIds : [
			''a336caa93a19233545578ce7f78b2fa678c5c60b''
		]
	},
	HiExampleCommit {
		#id : ''a336caa93a19233545578ce7f78b2fa678c5c60b'',
		#comment : ''refactored'',
		#parentIds : [
			''14601462d1767a51821125313648e2dd59b3932d''
		]
	},
	HiExampleCommit {
		#id : ''14601462d1767a51821125313648e2dd59b3932d'',
		#comment : ''add github, gitlab and bitbucket special repos'',
		#parentIds : [
			''586a5f7fdf31ab0e86d82aa3fa1e25bd37306713''
		]
	},
	HiExampleCommit {
		#id : ''586a5f7fdf31ab0e86d82aa3fa1e25bd37306713'',
		#comment : ''work on refreshing tools when something changed'',
		#parentIds : [
			''0ca7861bc21d1159cb130cd5acd204dcc9e5acaa''
		]
	},
	HiExampleCommit {
		#id : ''0ca7861bc21d1159cb130cd5acd204dcc9e5acaa'',
		#comment : ''add a couple of comments.'',
		#parentIds : [
			''36b1c6646e8d12f0e138e253d3dff9637fa6b9b6''
		]
	},
	HiExampleCommit {
		#id : ''36b1c6646e8d12f0e138e253d3dff9637fa6b9b6'',
		#comment : ''ensure text morphs are read only on tables'',
		#parentIds : [
			''4b32388161bb703eb8bf21b2dbad2e620f10d6ed''
		]
	},
	HiExampleCommit {
		#id : ''4b32388161bb703eb8bf21b2dbad2e620f10d6ed'',
		#comment : ''add "clone repository" dialog logic (was missing)'',
		#parentIds : [
			''7059f41df0c1eab1f5fb9db21dd5445187b5dade''
		]
	},
	HiExampleCommit {
		#id : ''7059f41df0c1eab1f5fb9db21dd5445187b5dade'',
		#comment : ''url must be taken from subclasses, otherwise it will throw a #subclassResposibility error instead a not found. '',
		#parentIds : [
			''5b1739ea8d154b71bac3c9f14738f390a4712651''
		]
	},
	HiExampleCommit {
		#id : ''5b1739ea8d154b71bac3c9f14738f390a4712651'',
		#comment : ''clean up'',
		#parentIds : [
			''31974590a2724b503d6efbeee985f7371f9bb8e1''
		]
	},
	HiExampleCommit {
		#id : ''31974590a2724b503d6efbeee985f7371f9bb8e1'',
		#comment : ''add help for toolbar items'',
		#parentIds : [
			''804f2b33d349f25414349b1888ab65268b8e2a13''
		]
	},
	HiExampleCommit {
		#id : ''804f2b33d349f25414349b1888ab65268b8e2a13'',
		#comment : ''apply IceTipReadOnlyMorph to see how it looks.'',
		#parentIds : [
			''37ddacc2003d2db811723c208ef1d0ff42a6578b''
		]
	},
	HiExampleCommit {
		#id : ''37ddacc2003d2db811723c208ef1d0ff42a6578b'',
		#comment : ''order to toolbars'',
		#parentIds : [
			''2a9340876afc2a5adfa79ec56080fdab78fc07b3''
		]
	},
	HiExampleCommit {
		#id : ''e158d78dc59ba05ef1c1a5b14a2a1c9e0595784a'',
		#comment : ''New changes model'',
		#parentIds : [
			''d41bf424c9649504907ec517277f77cbfce28552''
		]
	},
	HiExampleCommit {
		#id : ''2a9340876afc2a5adfa79ec56080fdab78fc07b3'',
		#comment : ''contexts and menus on tool GUI'',
		#parentIds : [
			''1c224fa8ec3c78cf8ecfd59cb07ef6e7524e4e90''
		]
	},
	HiExampleCommit {
		#id : ''d41bf424c9649504907ec517277f77cbfce28552'',
		#comment : ''History should show the commits in the right order'',
		#parentIds : [
			''5038955c4208f7e13caace5cd2bd1614f9060824''
		]
	},
	HiExampleCommit {
		#id : ''5038955c4208f7e13caace5cd2bd1614f9060824'',
		#comment : ''compatibleUsername should be available also for unknown commits'',
		#parentIds : [
			''b744581505092d44e5b12a8dba72c34c1fb13dbe''
		]
	},
	HiExampleCommit {
		#id : ''1c224fa8ec3c78cf8ecfd59cb07ef6e7524e4e90'',
		#comment : ''force borderColor to be transparent'',
		#parentIds : [
			''a3d2b6c5f7100592974d2eb0579e921e653a9c94''
		]
	},
	HiExampleCommit {
		#id : ''a3d2b6c5f7100592974d2eb0579e921e653a9c94'',
		#comment : ''left side and right side now are no longer handled by spec because it forces a proportional layout.'',
		#parentIds : [
			''67f2c7ac91001f3560cb3eff5e9e40c2ae8c6d7d''
		]
	},
	HiExampleCommit {
		#id : ''67f2c7ac91001f3560cb3eff5e9e40c2ae8c6d7d'',
		#comment : ''toolbars, tools... moving on with the new spec UI '',
		#parentIds : [
			''a3b89438b9ccbd8485e2dd7e75a39bb4aab5185c''
		]
	},
	HiExampleCommit {
		#id : ''b744581505092d44e5b12a8dba72c34c1fb13dbe'',
		#comment : ''Should not filter to get only loaded packages'',
		#parentIds : [
			''ef04a8a6522e648748a7eeb4bbd0284cc2859d18''
		]
	},
	HiExampleCommit {
		#id : ''a3b89438b9ccbd8485e2dd7e75a39bb4aab5185c'',
		#comment : ''fix a problem on throwing duplicate. \nI need the resignal because I want to return from here, not from inside the validation. But I want to be able to refactor validation in a proper way, so I prefer this uglyness over create a long method. '',
		#parentIds : [
			''171d5781285fb80e18b9049540d54152963eb107''
		]
	},
	HiExampleCommit {
		#id : ''171d5781285fb80e18b9049540d54152963eb107'',
		#comment : ''clean'',
		#parentIds : [
			''9e4ea7a234c878ed0c067cfc5ad659ef35cb69ca''
		]
	},
	HiExampleCommit {
		#id : ''9e4ea7a234c878ed0c067cfc5ad659ef35cb69ca'',
		#comment : ''a working version of a toolbar'',
		#parentIds : [
			''ca1f2c5816b34e122312f8cf015a24714a165f9e''
		]
	},
	HiExampleCommit {
		#id : ''ef04a8a6522e648748a7eeb4bbd0284cc2859d18'',
		#comment : ''Making a pass on the menu'',
		#parentIds : [
			''0e8ff2f074284c8a26aa41c9cb7f556360807ef7''
		]
	},
	HiExampleCommit {
		#id : ''0e8ff2f074284c8a26aa41c9cb7f556360807ef7'',
		#comment : ''Remove refresh menu actions'',
		#parentIds : [
			''715499ff8f305933e086a3adc4e2a8010a937ae8''
		]
	},
	HiExampleCommit {
		#id : ''715499ff8f305933e086a3adc4e2a8010a937ae8'',
		#comment : ''Rename IceSavedPackage into IcePackage'',
		#parentIds : [
			''fc8fc84f843e51c05b7df9863157d0ce6aa14600''
		]
	},
	HiExampleCommit {
		#id : ''fc8fc84f843e51c05b7df9863157d0ce6aa14600'',
		#comment : ''PATCH: detect when the diff against the working copy has no changes and mark them as not modified.'',
		#parentIds : [
			''017cf22f09757140c548935f4dc60a39e0ca28ce''
		]
	},
	HiExampleCommit {
		#id : ''017cf22f09757140c548935f4dc60a39e0ca28ce'',
		#comment : ''Fix history accessor '',
		#parentIds : [
			''d892a516a9b68345e1f69c41defeb9d269137b88''
		]
	},
	HiExampleCommit {
		#id : ''d892a516a9b68345e1f69c41defeb9d269137b88'',
		#comment : ''Adding access to git file system'',
		#parentIds : [
			''59da6fc6721774a129cc6d7064c2fb1f32cb56bb''
		]
	},
	HiExampleCommit {
		#id : ''0a781c949148084b2bb4d12f650220492d989be5'',
		#comment : ''Merge pull request #575 from martinmcclure/Issue574\n\nIssue574'',
		#parentIds : [
			''20eaebcfdb3ba94cd55e9ed48c2cbf300236d620'',
			''5c9c803b5ae256e7cf6610a7d247e44785d2de3b''
		]
	},
	HiExampleCommit {
		#id : ''ca1f2c5816b34e122312f8cf015a24714a165f9e'',
		#comment : ''fix PR analysis tool'',
		#parentIds : [
			''8887489a1187893f6777a903a7dcf90d84b1a3b2''
		]
	},
	HiExampleCommit {
		#id : ''8887489a1187893f6777a903a7dcf90d84b1a3b2'',
		#comment : ''fix #577'',
		#parentIds : [
			''52d778be3c4230abf6170001ae9b0ac3d7b5fa00''
		]
	},
	HiExampleCommit {
		#id : ''52d778be3c4230abf6170001ae9b0ac3d7b5fa00'',
		#comment : ''add couple of missing methods'',
		#parentIds : [
			''0a0f793430b488146854ce898140c5c3bf777adb''
		]
	},
	HiExampleCommit {
		#id : ''0a0f793430b488146854ce898140c5c3bf777adb'',
		#comment : ''maybe I need to answer an empty snapshot if there is no mcversion'',
		#parentIds : [
			''b8b196bbe1966ccc27a5db1f69df3d535435a03d''
		]
	},
	HiExampleCommit {
		#id : ''b8b196bbe1966ccc27a5db1f69df3d535435a03d'',
		#comment : ''work on the new UI using spec\nwork on the new toolbar'',
		#parentIds : [
			''5cf40b05b58ca3d08d6909c0fd514aa3221f59a2''
		]
	},
	HiExampleCommit {
		#id : ''59da6fc6721774a129cc6d7064c2fb1f32cb56bb'',
		#comment : ''Cleanup:\n - all tests except metacello integration are green\n - removed IcePackageReference (inlined into IceSavedPackage)'',
		#parentIds : [
			''8fada5ad9ce4b38c60eed0b0c22fa594835a0d47''
		]
	},
	HiExampleCommit {
		#id : ''8fada5ad9ce4b38c60eed0b0c22fa594835a0d47'',
		#comment : ''Fixing tests'',
		#parentIds : [
			''0d829d9e0202ed21f432656f815924af51e8c97f''
		]
	},
	HiExampleCommit {
		#id : ''20eaebcfdb3ba94cd55e9ed48c2cbf300236d620'',
		#comment : ''Merge pull request #578 from pharo-vcs/dev-0.6\n\nrelease 0.6.8'',
		#parentIds : [
			''5e2280ee36fb55770e35a7891c48b034a479d55b'',
			''23f5bb82c190dc05ab3fcc6e10afff47e957e9c5''
		]
	},
	HiExampleCommit {
		#id : ''23f5bb82c190dc05ab3fcc6e10afff47e957e9c5'',
		#comment : ''fix #577'',
		#parentIds : [
			''14b8d9bcb06260872a71f868d0196ef1377a174c''
		]
	},
	HiExampleCommit {
		#id : ''5cf40b05b58ca3d08d6909c0fd514aa3221f59a2'',
		#comment : ''add a generic read only form to allow info to be presented in a decent way.'',
		#parentIds : [
			''1faf508d41d0fbb6a32dbc8c68ce04f2c1f82f78''
		]
	},
	HiExampleCommit {
		#id : ''0d829d9e0202ed21f432656f815924af51e8c97f'',
		#comment : ''Adding cases for "New repository"'',
		#parentIds : [
			''d09f94fe23b6aa8c534dfa6d36b5a9beb797e834''
		]
	},
	HiExampleCommit {
		#id : ''1faf508d41d0fbb6a32dbc8c68ce04f2c1f82f78'',
		#comment : ''adding commands to packages browser.\nstill refactoring.'',
		#parentIds : [
			''a248e85934e5693a4922a044a3615274e1f5ff01''
		]
	},
	HiExampleCommit {
		#id : ''a248e85934e5693a4922a044a3615274e1f5ff01'',
		#comment : ''more refactors\nwork on packages browser'',
		#parentIds : [
			''bef7ac3e3fac7b7c4e32d43ccc366230e45294ca''
		]
	},
	HiExampleCommit {
		#id : ''bef7ac3e3fac7b7c4e32d43ccc366230e45294ca'',
		#comment : ''add a simple packages browser, refactor.'',
		#parentIds : [
			''f747c5ff60f48d0ea88c7c2e56c7a9fba478f193''
		]
	},
	HiExampleCommit {
		#id : ''f747c5ff60f48d0ea88c7c2e56c7a9fba478f193'',
		#comment : ''re-add error solving logic (and repackaged).\nadd a couple of commands.'',
		#parentIds : [
			''3489c280a2932e2d7646fb34cf3863b303c9c136''
		]
	},
	HiExampleCommit {
		#id : ''d09f94fe23b6aa8c534dfa6d36b5a9beb797e834'',
		#comment : ''Add debugging plugin'',
		#parentIds : [
			''510c736a4b9376dc28f6f0deff01d30a628e059a''
		]
	},
	HiExampleCommit {
		#id : ''510c736a4b9376dc28f6f0deff01d30a628e059a'',
		#comment : ''Adding class comments.'',
		#parentIds : [
			''3d68ae2949a5eb9328ce4e5b53d3c2059fa71e3a''
		]
	},
	HiExampleCommit {
		#id : ''3d68ae2949a5eb9328ce4e5b53d3c2059fa71e3a'',
		#comment : ''Add History object to optimize History Browser'',
		#parentIds : [
			''158b8cbaf4e1684f855bb7ca2b8654e5beff1055''
		]
	},
	HiExampleCommit {
		#id : ''158b8cbaf4e1684f855bb7ca2b8654e5beff1055'',
		#comment : ''Simplify commitAt: lookupCommit: to have a single entry point.'',
		#parentIds : [
			''088985d67480e66f0be35e5ef8c1a24e037824b0''
		]
	},
	HiExampleCommit {
		#id : ''088985d67480e66f0be35e5ef8c1a24e037824b0'',
		#comment : ''Fix in metacello integration for already cloned projects'',
		#parentIds : [
			''b3946c8bf1fd5508d7cfa1b062dc144639941f4f''
		]
	},
	HiExampleCommit {
		#id : ''b3946c8bf1fd5508d7cfa1b062dc144639941f4f'',
		#comment : ''Refactor to support Unknown Commit working copy state.'',
		#parentIds : [
			''2f6e8ead85fdc8b978a6b4aeb64a9be44c11b29a''
		]
	},
	HiExampleCommit {
		#id : ''2f6e8ead85fdc8b978a6b4aeb64a9be44c11b29a'',
		#comment : ''Optimize merge.\n\nShould only snapshot changed packages'',
		#parentIds : [
			''1711d0bb1034c252991bc0d4801a3668ab32212a''
		]
	},
	HiExampleCommit {
		#id : ''1711d0bb1034c252991bc0d4801a3668ab32212a'',
		#comment : ''Restoring optimization.\n\nCalculating changed packages with working copy MUST first calculate changes between the reference commit and the merged commit. Then we must also add the dirty packages. Otherwise we may unintentionally skip modified packages and overwrite changes.'',
		#parentIds : [
			''5e70d6f760a6d65c1a8339c064dabd461518b41a''
		]
	},
	HiExampleCommit {
		#id : ''5e70d6f760a6d65c1a8339c064dabd461518b41a'',
		#comment : ''Removed unused IceGitDiff'',
		#parentIds : [
			''d5fbe85405113e6b0e2671c1456f89367375af2b''
		]
	},
	HiExampleCommit {
		#id : ''d5fbe85405113e6b0e2671c1456f89367375af2b'',
		#comment : ''Fix on merge\n\nDo not optimize the selection of packages in here with #changedPackagesTo: otherwise we may end up merging only to changed packages, which is not right'',
		#parentIds : [
			''c10135a3bc3aee79a1943eeb70c6010772c578b1''
		]
	},
	HiExampleCommit {
		#id : ''c10135a3bc3aee79a1943eeb70c6010772c578b1'',
		#comment : ''Fixing tests'',
		#parentIds : [
			''50833911b174063a87fdb55b44e5c53869418f53''
		]
	},
	HiExampleCommit {
		#id : ''50833911b174063a87fdb55b44e5c53869418f53'',
		#comment : ''Recovering more lost changes. It starts to work better now.'',
		#parentIds : [
			''f8441f9790e101c4cd4d205c002a9a8c7b072327''
		]
	},
	HiExampleCommit {
		#id : ''f8441f9790e101c4cd4d205c002a9a8c7b072327'',
		#comment : ''Comments'',
		#parentIds : [
			''cf4a213811597b7b2a5b5d25a6592a144e11f1d8''
		]
	},
	HiExampleCommit {
		#id : ''cf4a213811597b7b2a5b5d25a6592a144e11f1d8'',
		#comment : ''Removed remaining "Last Fetched" in remote model'',
		#parentIds : [
			''1a059f81a7d2e06932a2428f626804174a62a8c4''
		]
	},
	HiExampleCommit {
		#id : ''1a059f81a7d2e06932a2428f626804174a62a8c4'',
		#comment : ''I like more "detached" that "outdated" (because status can mean "in a different possition", not just "with an older copy")'',
		#parentIds : [
			''3011a25e49ec68d7d5b78644760d3d58fec12490''
		]
	},
	HiExampleCommit {
		#id : ''3011a25e49ec68d7d5b78644760d3d58fec12490'',
		#comment : ''restore missing methods'',
		#parentIds : [
			''adaef3f5be5ed33cb32d1fbbc3530b98dccd2292''
		]
	},
	HiExampleCommit {
		#id : ''adaef3f5be5ed33cb32d1fbbc3530b98dccd2292'',
		#comment : ''using ring instead ring2'',
		#parentIds : [
			''91ea785b0c07e709cfc07c3a72420825f73bbe1f''
		]
	},
	HiExampleCommit {
		#id : ''91ea785b0c07e709cfc07c3a72420825f73bbe1f'',
		#comment : ''restore method missing'',
		#parentIds : [
			''0e811c482f21c53b897574141394afa11a67ec1d''
		]
	},
	HiExampleCommit {
		#id : ''0e811c482f21c53b897574141394afa11a67ec1d'',
		#comment : ''fix create pull request option '',
		#parentIds : [
			''dc19fed9492bf209dde1a504b0e9009051569841''
		]
	},
	HiExampleCommit {
		#id : ''dc19fed9492bf209dde1a504b0e9009051569841'',
		#comment : ''fix tool, but it still does not works super fine (shows "no changes" when it should load packages)'',
		#parentIds : [
			''52cf7b9f0341424ba310e96e38f175f0f9e08692''
		]
	},
	HiExampleCommit {
		#id : ''52cf7b9f0341424ba310e96e38f175f0f9e08692'',
		#comment : ''Fix merge with foreign package'',
		#parentIds : [
			''fd0d1facfca4c776ecfe97c19986e773e82cdf00''
		]
	},
	HiExampleCommit {
		#id : ''fd0d1facfca4c776ecfe97c19986e773e82cdf00'',
		#comment : ''Fix add package'',
		#parentIds : [
			''082a730a03120d123766b63f8090cb01dfb7e0fe''
		]
	},
	HiExampleCommit {
		#id : ''082a730a03120d123766b63f8090cb01dfb7e0fe'',
		#comment : ''fix merging when branches change and everything is messy ;)'',
		#parentIds : [
			''6babc33d359eaafff986bf5ee72418d5ced118b0''
		]
	},
	HiExampleCommit {
		#id : ''6babc33d359eaafff986bf5ee72418d5ced118b0'',
		#comment : ''- Failing loading of existing repository (missing #obtainRepositoryName)\n- Some fixes on branch management'',
		#parentIds : [
			''6eb671cf07316ef9fecc1a37c3ea3851d32d69a1''
		]
	},
	HiExampleCommit {
		#id : ''6eb671cf07316ef9fecc1a37c3ea3851d32d69a1'',
		#comment : ''Manage merge with multiple packages in merge state'',
		#parentIds : [
			''3378490452b22d4005cfd40e5e313538a63f3112''
		]
	},
	HiExampleCommit {
		#id : ''3378490452b22d4005cfd40e5e313538a63f3112'',
		#comment : ''Fix glitches in package loading'',
		#parentIds : [
			''aa77e26270f91ecadc1c8768a8343800ecd12f26''
		]
	},
	HiExampleCommit {
		#id : ''aa77e26270f91ecadc1c8768a8343800ecd12f26'',
		#comment : ''add missing diff methods'',
		#parentIds : [
			''ebe739cfc251bf0da674696c694ea0b60644e6a4''
		]
	},
	HiExampleCommit {
		#id : ''ebe739cfc251bf0da674696c694ea0b60644e6a4'',
		#comment : ''replace #isLoaded with #isMerged'',
		#parentIds : [
			''fba27190c26dfbc33e3ce0e9eecd5eee0258dbbb''
		]
	},
	HiExampleCommit {
		#id : ''fba27190c26dfbc33e3ce0e9eecd5eee0258dbbb'',
		#comment : ''history *should* be working :)'',
		#parentIds : [
			''497c73e25b90a8b1fb02e6015692bda187a19ec5''
		]
	},
	HiExampleCommit {
		#id : ''497c73e25b90a8b1fb02e6015692bda187a19ec5'',
		#comment : ''fix lookupCommit:ifAbsent:'',
		#parentIds : [
			''d5e108b7920ec8e8633a9e0c6cfeab92127805c8''
		]
	},
	HiExampleCommit {
		#id : ''d5e108b7920ec8e8633a9e0c6cfeab92127805c8'',
		#comment : ''change origin text'',
		#parentIds : [
			''7d96a9936f736b621c9c89ed83a7747a37f97d9a''
		]
	},
	HiExampleCommit {
		#id : ''7d96a9936f736b621c9c89ed83a7747a37f97d9a'',
		#comment : ''retrieve history shows a message.'',
		#parentIds : [
			''d515d8b26f98bf5409e85d67de480bd175a7b0d0''
		]
	},
	HiExampleCommit {
		#id : ''d515d8b26f98bf5409e85d67de480bd175a7b0d0'',
		#comment : ''fixed log of class side methods \nfixed log on detached heads\n'',
		#parentIds : [
			''38d199a59a996887ca46396e5e91f793fff43b64''
		]
	},
	HiExampleCommit {
		#id : ''38d199a59a996887ca46396e5e91f793fff43b64'',
		#comment : ''merged? I don\''t know why this methods are here.'',
		#parentIds : [
			''31abff780a53ae68f21434eaab1bfe834c233b88''
		]
	},
	HiExampleCommit {
		#id : ''31abff780a53ae68f21434eaab1bfe834c233b88'',
		#comment : ''log of methods for tonel and filetree (WIP) '',
		#parentIds : [
			''472dc5f1c7bfb69c1dcc58a6ab6d0751396cfda7''
		]
	},
	HiExampleCommit {
		#id : ''472dc5f1c7bfb69c1dcc58a6ab6d0751396cfda7'',
		#comment : ''More Comments'',
		#parentIds : [
			''ded1da2550b6685f02dc214fe6a274e3f7557f2a''
		]
	},
	HiExampleCommit {
		#id : ''ded1da2550b6685f02dc214fe6a274e3f7557f2a'',
		#comment : ''More coments'',
		#parentIds : [
			''0214a70d35736089220279a64ca3662563fed6b3''
		]
	},
	HiExampleCommit {
		#id : ''0214a70d35736089220279a64ca3662563fed6b3'',
		#comment : ''Cleanup and comments in WorkingCopy'',
		#parentIds : [
			''e89f59a59fc10c94bf2e9a122df87d895121548f''
		]
	},
	HiExampleCommit {
		#id : ''e89f59a59fc10c94bf2e9a122df87d895121548f'',
		#comment : ''More cleanups and comments'',
		#parentIds : [
			''214b34b8c4752ce6a3482475bbc97a0b79f0a991''
		]
	},
	HiExampleCommit {
		#id : ''214b34b8c4752ce6a3482475bbc97a0b79f0a991'',
		#comment : ''Cleanups and comments'',
		#parentIds : [
			''b9969517e3d77d0c17d99985be00098e5663f1fc''
		]
	},
	HiExampleCommit {
		#id : ''b9969517e3d77d0c17d99985be00098e5663f1fc'',
		#comment : ''Organizing working copy and package state'',
		#parentIds : [
			''5002fee6c9fbf5f4fb000a970887fb052b9619c9''
		]
	},
	HiExampleCommit {
		#id : ''5002fee6c9fbf5f4fb000a970887fb052b9619c9'',
		#comment : ''Cleanup unused classes and methods'',
		#parentIds : [
			''dad127752b675a92fcc9ba6a65fa03650378fa40''
		]
	},
	HiExampleCommit {
		#id : ''dad127752b675a92fcc9ba6a65fa03650378fa40'',
		#comment : ''- Optimize merge of large repositories\n- Managing non fetched commits\n- Managing the absence of pull remotes'',
		#parentIds : [
			''9c774deaa71b45aafdcea7cc45a4b7dd3beb3236''
		]
	},
	HiExampleCommit {
		#id : ''9c774deaa71b45aafdcea7cc45a4b7dd3beb3236'',
		#comment : ''Optimizing loading of Pharo repository'',
		#parentIds : [
			''ff70b5c5900a760304d61dbad28a4aec4ff2fcda''
		]
	},
	HiExampleCommit {
		#id : ''ff70b5c5900a760304d61dbad28a4aec4ff2fcda'',
		#comment : ''Removed IceTGitBranch\n\nPass on quick tests'',
		#parentIds : [
			''8e6f2d50e5eb21322385b068c62e9311d253f765''
		]
	},
	HiExampleCommit {
		#id : ''8e6f2d50e5eb21322385b068c62e9311d253f765'',
		#comment : ''adoptInstance: -> adoptCommit:'',
		#parentIds : [
			''da4d25bd773916f111c834f923c9434b342614bd''
		]
	},
	HiExampleCommit {
		#id : ''da4d25bd773916f111c834f923c9434b342614bd'',
		#comment : ''Pass on old iceberg tests'',
		#parentIds : [
			''77591d8613173bbd59c85345bc992bf17de67391''
		]
	},
	HiExampleCommit {
		#id : ''77591d8613173bbd59c85345bc992bf17de67391'',
		#comment : ''Preloading should unload everything\n'',
		#parentIds : [
			''fe817d5c99a1016ce7f62d47b112ee9a92254012''
		]
	},
	HiExampleCommit {
		#id : ''fe817d5c99a1016ce7f62d47b112ee9a92254012'',
		#comment : ''Only in pharo7 now\n'',
		#parentIds : [
			''c7dfc584998ac158c32b0faf3cfb379e12927a32''
		]
	},
	HiExampleCommit {
		#id : ''c7dfc584998ac158c32b0faf3cfb379e12927a32'',
		#comment : ''Fix metacello integration tests'',
		#parentIds : [
			''6f2b0dfd9e5bd28fadf9314c4b3d78ac48473f41''
		]
	},
	HiExampleCommit {
		#id : ''6f2b0dfd9e5bd28fadf9314c4b3d78ac48473f41'',
		#comment : ''Pass on original iceberg tests'',
		#parentIds : [
			''bc5029e8377a4345f941494bde1869bced85e99d''
		]
	},
	HiExampleCommit {
		#id : ''bc5029e8377a4345f941494bde1869bced85e99d'',
		#comment : ''Fixing wrong test'',
		#parentIds : [
			''c093d23c2e4f7efcb2ba65e6ba1a443477d5a751''
		]
	},
	HiExampleCommit {
		#id : ''c093d23c2e4f7efcb2ba65e6ba1a443477d5a751'',
		#comment : ''Fix unintentional creation of MC working copies'',
		#parentIds : [
			''ca2dfc42039d024220ece99784465136fef6dc31''
		]
	},
	HiExampleCommit {
		#id : ''ca2dfc42039d024220ece99784465136fef6dc31'',
		#comment : ''Implement adopt commit for fast-forward'',
		#parentIds : [
			''52462f1a9e36d3df0b02e7a472dc35c2e1fe917d''
		]
	},
	HiExampleCommit {
		#id : ''52462f1a9e36d3df0b02e7a472dc35c2e1fe917d'',
		#comment : ''Support for automatic merge fastforward and no conflict merge'',
		#parentIds : [
			''c710c559a7ec74f5971a826f078eb79bcbd29d8e''
		]
	},
	HiExampleCommit {
		#id : ''c710c559a7ec74f5971a826f078eb79bcbd29d8e'',
		#comment : ''Fix merges'',
		#parentIds : [
			''cd5c4fec568cdb0a21e708d951840740ded6d11c''
		]
	},
	HiExampleCommit {
		#id : ''cd5c4fec568cdb0a21e708d951840740ded6d11c'',
		#comment : ''Adding more tests'',
		#parentIds : [
			''ff1f01cbba36b20288dc77d13a2d2928e4c75a33''
		]
	},
	HiExampleCommit {
		#id : ''ff1f01cbba36b20288dc77d13a2d2928e4c75a33'',
		#comment : ''Calculating the state of the working copy from the state of the packages in it.\n'',
		#parentIds : [
			''949b622b2761bc38e5c86c29190282aec4c6d267''
		]
	},
	HiExampleCommit {
		#id : ''949b622b2761bc38e5c86c29190282aec4c6d267'',
		#comment : ''Merge'',
		#parentIds : [
			''619a7fbba7d83560ba85811341f46e482dbc8d0e''
		]
	},
	HiExampleCommit {
		#id : ''619a7fbba7d83560ba85811341f46e482dbc8d0e'',
		#comment : ''No need to check for rpackage existance. This method is only called on loaded packages.'',
		#parentIds : [
			''6b937a19dd00a6cf5542686966062ffff0b2a846''
		]
	},
	HiExampleCommit {
		#id : ''6b937a19dd00a6cf5542686966062ffff0b2a846'',
		#comment : ''Updating packages only for loaded packages'',
		#parentIds : [
			''5d437849ef0391cb1b498d83216d5517a3f4c649''
		]
	},
	HiExampleCommit {
		#id : ''5d437849ef0391cb1b498d83216d5517a3f4c649'',
		#comment : ''Better managing commits in the working copy'',
		#parentIds : [
			''2cf2ad7e98150c1ce8d754c3d914a3c15176290e''
		]
	},
	HiExampleCommit {
		#id : ''2cf2ad7e98150c1ce8d754c3d914a3c15176290e'',
		#comment : ''Added initial tests about working copy states'',
		#parentIds : [
			''77eb41c4ea2f8ad2db574387c830a4689ee36526''
		]
	},
	HiExampleCommit {
		#id : ''77eb41c4ea2f8ad2db574387c830a4689ee36526'',
		#comment : ''Tests are green'',
		#parentIds : [
			''678efeff4afd9d61bd3f3cbeb6fefe38b6385f5e''
		]
	},
	HiExampleCommit {
		#id : ''678efeff4afd9d61bd3f3cbeb6fefe38b6385f5e'',
		#comment : ''Fix 4 failing tests due to the creation of packages in working copy'',
		#parentIds : [
			''bbc248bb671587356900120bd8e38ee50544f1bd''
		]
	},
	HiExampleCommit {
		#id : ''bbc248bb671587356900120bd8e38ee50544f1bd'',
		#comment : ''Removing last fetched data.'',
		#parentIds : [
			''63578e92ca6217bb89cf2774b2036f6342271517''
		]
	},
	HiExampleCommit {
		#id : ''63578e92ca6217bb89cf2774b2036f6342271517'',
		#comment : ''Removing cache of incoming commits'',
		#parentIds : [
			''1cef7b55aec8eb0b1699fdc37d5f32e8674e8565''
		]
	},
	HiExampleCommit {
		#id : ''1cef7b55aec8eb0b1699fdc37d5f32e8674e8565'',
		#comment : ''Adding IceRepositoryModel.'',
		#parentIds : [
			''75d85d76eec7903a7acd86cdb9539097b69322d0''
		]
	},
	HiExampleCommit {
		#id : ''75d85d76eec7903a7acd86cdb9539097b69322d0'',
		#comment : ''Fixing reverts and getting commit in branch.'',
		#parentIds : [
			''9736f17475c5cfb61e927e99a64d93e3a5e5bedf''
		]
	},
	HiExampleCommit {
		#id : ''9736f17475c5cfb61e927e99a64d93e3a5e5bedf'',
		#comment : ''Adding cache for outgoing and incoming commits.\nThe cache is only invalidated on refresh.'',
		#parentIds : [
			''2863e34964fd871f1646c3a5d8e9f58f2b0030d2''
		]
	},
	HiExampleCommit {
		#id : ''2863e34964fd871f1646c3a5d8e9f58f2b0030d2'',
		#comment : ''Fixing version of libgit2.\nFixing the display string in remotes.'',
		#parentIds : [
			''02e849dc5e4dfc6b9bbd2e9678a3b5427f92c321''
		]
	},
	HiExampleCommit {
		#id : ''02e849dc5e4dfc6b9bbd2e9678a3b5427f92c321'',
		#comment : ''Fixing dependency to Ring2 and description in Metacello integration'',
		#parentIds : [
			''73c636d6743db6ae37632828a4db0cd41b0b5cfb''
		]
	},
	HiExampleCommit {
		#id : ''73c636d6743db6ae37632828a4db0cd41b0b5cfb'',
		#comment : ''Speed up tests using Ring2'',
		#parentIds : [
			''0c69871d8bd6e89cd38e0fcd4138f4eafa35d684''
		]
	},
	HiExampleCommit {
		#id : ''0c69871d8bd6e89cd38e0fcd4138f4eafa35d684'',
		#comment : ''Remove halt in commit'',
		#parentIds : [
			''8427c210e57226491a801cd0d14587cd56a3b1d2''
		]
	},
	HiExampleCommit {
		#id : ''8427c210e57226491a801cd0d14587cd56a3b1d2'',
		#comment : ''Experiments with Spec UI'',
		#parentIds : [
			''2235be64b296a2dbecfcc119ce74389205c635fc''
		]
	},
	HiExampleCommit {
		#id : ''2235be64b296a2dbecfcc119ce74389205c635fc'',
		#comment : ''Removed old classes'',
		#parentIds : [
			''5ba845574f45f03ea4672ee02566d90b3d8938bc''
		]
	},
	HiExampleCommit {
		#id : ''5ba845574f45f03ea4672ee02566d90b3d8938bc'',
		#comment : ''- Extending memory backend with examples\n- Added first Smart UI version'',
		#parentIds : [
			''cc0cfa83defe012f5e83db2e4084482075ae78fe''
		]
	},
	HiExampleCommit {
		#id : ''cc0cfa83defe012f5e83db2e4084482075ae78fe'',
		#comment : ''Rename old classes with Old prefix.\nRemoving *new* prefix from new classes.'',
		#parentIds : [
			''be5b575a5bc004a5b86cabff78ec1ae03b51cbe0''
		]
	},
	HiExampleCommit {
		#id : ''be5b575a5bc004a5b86cabff78ec1ae03b51cbe0'',
		#comment : ''Added new Smart UI statuses'',
		#parentIds : [
			''7d01ee0daa4e400bd7ab302200b9dbb7d184a282''
		]
	},
	HiExampleCommit {
		#id : ''7d01ee0daa4e400bd7ab302200b9dbb7d184a282'',
		#comment : ''Smart UI'',
		#parentIds : [
			''6380245ccfc145e4b3576fc543aa3b4745e735e7''
		]
	},
	HiExampleCommit {
		#id : ''6380245ccfc145e4b3576fc543aa3b4745e735e7'',
		#comment : ''Add Detached HEAD and Detached Working Copy states'',
		#parentIds : [
			''2d3ab4f46bf6abf326e7da8899622714ed9faea1''
		]
	},
	HiExampleCommit {
		#id : ''2d3ab4f46bf6abf326e7da8899622714ed9faea1'',
		#comment : ''Fix diff'',
		#parentIds : [
			''022aa9d95c9c47901410b7adfe659a9f5246cc70''
		]
	},
	HiExampleCommit {
		#id : ''022aa9d95c9c47901410b7adfe659a9f5246cc70'',
		#comment : ''Fix repository creation'',
		#parentIds : [
			''c00f1838623c850394bbf4b74e08636bce67a279''
		]
	},
	HiExampleCommit {
		#id : ''c00f1838623c850394bbf4b74e08636bce67a279'',
		#comment : ''Fix the full diff'',
		#parentIds : [
			''a43687ca0515d4a450db3305bbda44d29d4a7043''
		]
	},
	HiExampleCommit {
		#id : ''a43687ca0515d4a450db3305bbda44d29d4a7043'',
		#comment : ''Fix baseline to point to Iceberg-Memory'',
		#parentIds : [
			''2d00fc64d5e14ca7561965e69549d97b61eb1934''
		]
	},
	HiExampleCommit {
		#id : ''2d00fc64d5e14ca7561965e69549d97b61eb1934'',
		#comment : ''Added Iceberg-Memory for testing purposes'',
		#parentIds : [
			''ce6688b9adc2f2775a5f8246dc33d1f502ee4bb3''
		]
	},
	HiExampleCommit {
		#id : ''ce6688b9adc2f2775a5f8246dc33d1f502ee4bb3'',
		#comment : ''Enhance loaded version description for new repositories'',
		#parentIds : [
			''eae12ac8257f78deaef82583ec4f4ebfc0694a07''
		]
	},
	HiExampleCommit {
		#id : ''eae12ac8257f78deaef82583ec4f4ebfc0694a07'',
		#comment : ''Place halt  to capture cherry-picking'',
		#parentIds : [
			''e1ccf0fbcc017861b2ead18860ac310de10d30d3''
		]
	},
	HiExampleCommit {
		#id : ''e1ccf0fbcc017861b2ead18860ac310de10d30d3'',
		#comment : ''Fix commit comparison for iceberg UI'',
		#parentIds : [
			''6710c34b2e5cd8040555ba31aa21632ad25b7d2e''
		]
	},
	HiExampleCommit {
		#id : ''6710c34b2e5cd8040555ba31aa21632ad25b7d2e'',
		#comment : ''Changes needed to push'',
		#parentIds : [
			''fca64aebab3f5353251eccf6e9d3d8dc9569f92d''
		]
	},
	HiExampleCommit {
		#id : ''fca64aebab3f5353251eccf6e9d3d8dc9569f92d'',
		#comment : ''Heavy refactoring\n - added tests\n - multiple backends\n - review API and main class names'',
		#parentIds : [
			''5e2280ee36fb55770e35a7891c48b034a479d55b''
		]
	},
	HiExampleCommit {
		#id : ''5c9c803b5ae256e7cf6610a7d247e44785d2de3b'',
		#comment : ''#projectPath now allows for the possibility of the owner being nil.\n'',
		#parentIds : [
			''9da5264d7763fec8f108e421857a03be972646aa''
		]
	},
	HiExampleCommit {
		#id : ''9da5264d7763fec8f108e421857a03be972646aa'',
		#comment : ''Fix for Gitolite-type owner-less URLs, Iceberg issue #574, Pharo case 20895.\n'',
		#parentIds : [
			''66cf83851337b96f222d1b9e42546e9346554c3a''
		]
	},
	HiExampleCommit {
		#id : ''66cf83851337b96f222d1b9e42546e9346554c3a'',
		#comment : ''Add tests for Gitolite-type owner-less URLs, Iceberg issue #574, Pharo case 20895.\n'',
		#parentIds : [
			''5e2280ee36fb55770e35a7891c48b034a479d55b''
		]
	},
	HiExampleCommit {
		#id : ''3489c280a2932e2d7646fb34cf3863b303c9c136'',
		#comment : ''refactors and add some ito the diverse presenters'',
		#parentIds : [
			''9e94d995d0248e5033f28c40926891d18528c1e6''
		]
	},
	HiExampleCommit {
		#id : ''9e94d995d0248e5033f28c40926891d18528c1e6'',
		#comment : ''work on add projects'',
		#parentIds : [
			''a7ce03f2bc97dd530272f87a79b7f410183c6e15''
		]
	},
	HiExampleCommit {
		#id : ''a7ce03f2bc97dd530272f87a79b7f410183c6e15'',
		#comment : ''working with commands (taken from ccommander project)'',
		#parentIds : [
			''1df0853171965fb179981905cab185d00b561fc7''
		]
	},
	HiExampleCommit {
		#id : ''5e2280ee36fb55770e35a7891c48b034a479d55b'',
		#comment : ''Merge pull request #571 from pharo-vcs/dev-0.6\n\nv0.6.7'',
		#parentIds : [
			''f084a8f762fc1bd6975a4adc69b0d253dbf2dc54'',
			''14b8d9bcb06260872a71f868d0196ef1377a174c''
		]
	},
	HiExampleCommit {
		#id : ''14b8d9bcb06260872a71f868d0196ef1377a174c'',
		#comment : ''updated version for update'',
		#parentIds : [
			''cf813e1df62e4c86a64583bf39a9ef3e1a614dac''
		]
	},
	HiExampleCommit {
		#id : ''cf813e1df62e4c86a64583bf39a9ef3e1a614dac'',
		#comment : ''use tonel 1.0.6'',
		#parentIds : [
			''92ba391dd6cc95c1f8991a09220a914d580f1a11''
		]
	},
	HiExampleCommit {
		#id : ''92ba391dd6cc95c1f8991a09220a914d580f1a11'',
		#comment : ''use libgit 1.0.5'',
		#parentIds : [
			''85aa3f8734e3e5bc5550177f14e70057a1f80251''
		]
	},
	HiExampleCommit {
		#id : ''1df0853171965fb179981905cab185d00b561fc7'',
		#comment : ''add #useIncoming/#useLoad strategy when loading from metacello'',
		#parentIds : [
			''9dc7810630dd879ff3b846944006c868663b34bb''
		]
	},
	HiExampleCommit {
		#id : ''9dc7810630dd879ff3b846944006c868663b34bb'',
		#comment : ''merged'',
		#parentIds : [
			''d8d3ef80d0f4b2c77752d735d7a3d4b25513acd2''
		]
	},
	HiExampleCommit {
		#id : ''d8d3ef80d0f4b2c77752d735d7a3d4b25513acd2'',
		#comment : ''refactor announcements'',
		#parentIds : [
			''11d8f1c1a33b1b417ac20614ba0abf1bd4426862''
		]
	},
	HiExampleCommit {
		#id : ''11d8f1c1a33b1b417ac20614ba0abf1bd4426862'',
		#comment : ''Add debugging plugin'',
		#parentIds : [
			''ca127b11500032f3fc5e3fb7300254523c2187ff''
		]
	},
	HiExampleCommit {
		#id : ''ca127b11500032f3fc5e3fb7300254523c2187ff'',
		#comment : ''Adding class comments.'',
		#parentIds : [
			''154997e89e27f925e9e2e3055954b35b60127c1d''
		]
	},
	HiExampleCommit {
		#id : ''154997e89e27f925e9e2e3055954b35b60127c1d'',
		#comment : ''Add History object to optimize History Browser'',
		#parentIds : [
			''31de7f3cdcc89576dcf48d885e97c5dcf78c62a0''
		]
	},
	HiExampleCommit {
		#id : ''31de7f3cdcc89576dcf48d885e97c5dcf78c62a0'',
		#comment : ''Simplify commitAt: lookupCommit: to have a single entry point.'',
		#parentIds : [
			''b4dff418f053704bd5db419f54dbbb58454c4f55''
		]
	},
	HiExampleCommit {
		#id : ''b4dff418f053704bd5db419f54dbbb58454c4f55'',
		#comment : ''Fix in metacello integration for already cloned projects'',
		#parentIds : [
			''d5941520a71f027e4dd1cc2cc35fc3f69ec7fb1e''
		]
	},
	HiExampleCommit {
		#id : ''d5941520a71f027e4dd1cc2cc35fc3f69ec7fb1e'',
		#comment : ''Refactor to support Unknown Commit working copy state.'',
		#parentIds : [
			''1c91518424c0aa30aa2722424a80a1ae3b7e1923''
		]
	},
	HiExampleCommit {
		#id : ''1c91518424c0aa30aa2722424a80a1ae3b7e1923'',
		#comment : ''Optimize merge.\n\nShould only snapshot changed packages'',
		#parentIds : [
			''c3f3b14cee7e57201e7dd0f30263e48a04acb63d''
		]
	},
	HiExampleCommit {
		#id : ''c3f3b14cee7e57201e7dd0f30263e48a04acb63d'',
		#comment : ''Restoring optimization.\n\nCalculating changed packages with working copy MUST first calculate changes between the reference commit and the merged commit. Then we must also add the dirty packages. Otherwise we may unintentionally skip modified packages and overwrite changes.'',
		#parentIds : [
			''0d9f8def1b2c158565a85438b8debbeea3ea3c5d''
		]
	},
	HiExampleCommit {
		#id : ''0d9f8def1b2c158565a85438b8debbeea3ea3c5d'',
		#comment : ''Removed unused IceGitDiff'',
		#parentIds : [
			''cc515e49be2a02f80519c85f956417d6c03cacd9''
		]
	},
	HiExampleCommit {
		#id : ''cc515e49be2a02f80519c85f956417d6c03cacd9'',
		#comment : ''Fix on merge\n\nDo not optimize the selection of packages in here with #changedPackagesTo: otherwise we may end up merging only to changed packages, which is not right'',
		#parentIds : [
			''3c399557063d0598e8f547b55d7c1a1fab7fc7ec''
		]
	},
	HiExampleCommit {
		#id : ''3c399557063d0598e8f547b55d7c1a1fab7fc7ec'',
		#comment : ''Fixing tests'',
		#parentIds : [
			''fe6be513b503e0e1a787380737683152c9e20ebb''
		]
	},
	HiExampleCommit {
		#id : ''fe6be513b503e0e1a787380737683152c9e20ebb'',
		#comment : ''Recovering more lost changes. It starts to work better now.'',
		#parentIds : [
			''e3a39a4b67c549bb5a20e59c7be704bb3ed216f7''
		]
	},
	HiExampleCommit {
		#id : ''e3a39a4b67c549bb5a20e59c7be704bb3ed216f7'',
		#comment : ''Comments'',
		#parentIds : [
			''f22895e9c85dcec9ba0f6125feb85ee713c7ac04''
		]
	},
	HiExampleCommit {
		#id : ''f22895e9c85dcec9ba0f6125feb85ee713c7ac04'',
		#comment : ''Removed remaining "Last Fetched" in remote model'',
		#parentIds : [
			''70f1a3d4cb99e8791f6adab06e59ce877012fd60''
		]
	},
	HiExampleCommit {
		#id : ''70f1a3d4cb99e8791f6adab06e59ce877012fd60'',
		#comment : ''using libgit 1.0.4'',
		#parentIds : [
			''642a9b7f9fd09635b2810533b55246022c9bb893''
		]
	},
	HiExampleCommit {
		#id : ''642a9b7f9fd09635b2810533b55246022c9bb893'',
		#comment : ''I like more "detached" that "outdated" (because status can mean "in a different possition", not just "with an older copy")'',
		#parentIds : [
			''8f6581ad7deb9d87ae1559c5901cf94031c5eb97''
		]
	},
	HiExampleCommit {
		#id : ''8f6581ad7deb9d87ae1559c5901cf94031c5eb97'',
		#comment : ''restore missing methods'',
		#parentIds : [
			''14f8439f6d750573a1b841a3c24326ced2faa6e9''
		]
	},
	HiExampleCommit {
		#id : ''14f8439f6d750573a1b841a3c24326ced2faa6e9'',
		#comment : ''using ring instead ring2'',
		#parentIds : [
			''e8c297a22fa5b574cc61ed9631fe30fe27c792ad''
		]
	},
	HiExampleCommit {
		#id : ''e8c297a22fa5b574cc61ed9631fe30fe27c792ad'',
		#comment : ''restore method missing'',
		#parentIds : [
			''4a50719cf6f83269754c5bee141764fe30900ea5''
		]
	},
	HiExampleCommit {
		#id : ''4a50719cf6f83269754c5bee141764fe30900ea5'',
		#comment : ''fix create pull request option '',
		#parentIds : [
			''4c89c0a09c2a373f9c0e94d84c4ab7a99579dff4''
		]
	},
	HiExampleCommit {
		#id : ''4c89c0a09c2a373f9c0e94d84c4ab7a99579dff4'',
		#comment : ''fix tool, but it still does not works super fine (shows "no changes" when it should load packages)'',
		#parentIds : [
			''f1af721c932a6494cdf1a8a1da7438a00fe42b41''
		]
	},
	HiExampleCommit {
		#id : ''f1af721c932a6494cdf1a8a1da7438a00fe42b41'',
		#comment : ''Fix merge with foreign package'',
		#parentIds : [
			''1b730da75f370065917aca418e50beba3a6c7fe0''
		]
	},
	HiExampleCommit {
		#id : ''1b730da75f370065917aca418e50beba3a6c7fe0'',
		#comment : ''Fix add package'',
		#parentIds : [
			''859e5b4334a6bdf8d559b10c036d451e9ef42be7''
		]
	},
	HiExampleCommit {
		#id : ''859e5b4334a6bdf8d559b10c036d451e9ef42be7'',
		#comment : ''fix merging when branches change and everything is messy ;)'',
		#parentIds : [
			''dbe809220767bb6d5ce887cc4e289a57cee9651d''
		]
	},
	HiExampleCommit {
		#id : ''dbe809220767bb6d5ce887cc4e289a57cee9651d'',
		#comment : ''- Failing loading of existing repository (missing #obtainRepositoryName)\n- Some fixes on branch management'',
		#parentIds : [
			''ee4eebcfc6139f59355f166ecc7fbc879403398c''
		]
	},
	HiExampleCommit {
		#id : ''ee4eebcfc6139f59355f166ecc7fbc879403398c'',
		#comment : ''Manage merge with multiple packages in merge state'',
		#parentIds : [
			''0e83cadf41e7989e1950e3de0a4d65d87dd0b7d4''
		]
	},
	HiExampleCommit {
		#id : ''85aa3f8734e3e5bc5550177f14e70057a1f80251'',
		#comment : ''Merge pull request #560 from seandenigris/558-Gitlab-Private-Project-Support\n\n[Bug]: Rescue Missing Gitlab Iceberg-Adapters'',
		#parentIds : [
			''09bb31ab91bbb29cffe6ca68255774765f4d7704'',
			''91148cafcb3dc5674b92cfab2f470ffee169b06d''
		]
	},
	HiExampleCommit {
		#id : ''91148cafcb3dc5674b92cfab2f470ffee169b06d'',
		#comment : ''[Bug]: Rescue Missing Gitlab Iceberg-Adapters'',
		#parentIds : [
			''6b88e52c3977940f6c78f2cb187118d7d4830fdc''
		]
	},
	HiExampleCommit {
		#id : ''0e83cadf41e7989e1950e3de0a4d65d87dd0b7d4'',
		#comment : ''Fix glitches in package loading'',
		#parentIds : [
			''253e974699e76a88d829cdfd5a3b1524262956fa''
		]
	},
	HiExampleCommit {
		#id : ''253e974699e76a88d829cdfd5a3b1524262956fa'',
		#comment : ''add missing diff methods'',
		#parentIds : [
			''001a54782f50ee56933ae4e06190259f6aba62ff''
		]
	},
	HiExampleCommit {
		#id : ''001a54782f50ee56933ae4e06190259f6aba62ff'',
		#comment : ''replace #isLoaded with #isMerged'',
		#parentIds : [
			''f41d6308779878f75779946afe912b56ea2c35e8''
		]
	},
	HiExampleCommit {
		#id : ''f41d6308779878f75779946afe912b56ea2c35e8'',
		#comment : ''history *should* be working :)'',
		#parentIds : [
			''9bfda5321996ee199445567073d67ce580926735''
		]
	},
	HiExampleCommit {
		#id : ''9bfda5321996ee199445567073d67ce580926735'',
		#comment : ''fix lookupCommit:ifAbsent:'',
		#parentIds : [
			''679e7bc481cb78fb5042206e09692683440a6fa7''
		]
	},
	HiExampleCommit {
		#id : ''679e7bc481cb78fb5042206e09692683440a6fa7'',
		#comment : ''change origin text'',
		#parentIds : [
			''2c40bdb5e287f6ffcd9a03f36d80be07266a60d8''
		]
	},
	HiExampleCommit {
		#id : ''2c40bdb5e287f6ffcd9a03f36d80be07266a60d8'',
		#comment : ''retrieve history shows a message.'',
		#parentIds : [
			''d74ac198dd63c6ae7976e77a27c414e170ec5859''
		]
	},
	HiExampleCommit {
		#id : ''d74ac198dd63c6ae7976e77a27c414e170ec5859'',
		#comment : ''fixed log of class side methods \nfixed log on detached heads\n'',
		#parentIds : [
			''e5aace7906c17c2f57f24896d19297f8212a9d9d''
		]
	},
	HiExampleCommit {
		#id : ''e5aace7906c17c2f57f24896d19297f8212a9d9d'',
		#comment : ''merged? I don\''t know why this methods are here.'',
		#parentIds : [
			''d772e94bde6f2f037ed508990c9be15d7ef4c672''
		]
	},
	HiExampleCommit {
		#id : ''d772e94bde6f2f037ed508990c9be15d7ef4c672'',
		#comment : ''log of methods for tonel and filetree (WIP) '',
		#parentIds : [
			''4a749cc188892e9b34d9989a619757a8fafb6140''
		]
	},
	HiExampleCommit {
		#id : ''4a749cc188892e9b34d9989a619757a8fafb6140'',
		#comment : ''More Comments'',
		#parentIds : [
			''00b0e4d42c6d8d2b29bbd43bab6f6521c31292ae''
		]
	},
	HiExampleCommit {
		#id : ''00b0e4d42c6d8d2b29bbd43bab6f6521c31292ae'',
		#comment : ''More coments'',
		#parentIds : [
			''c1e1ed26161a29ff16ad334975e9f6b8be94d2ac''
		]
	},
	HiExampleCommit {
		#id : ''c1e1ed26161a29ff16ad334975e9f6b8be94d2ac'',
		#comment : ''Cleanup and comments in WorkingCopy'',
		#parentIds : [
			''ada28d563b46457d5bbdb4a926651a7fe89329dd''
		]
	},
	HiExampleCommit {
		#id : ''ada28d563b46457d5bbdb4a926651a7fe89329dd'',
		#comment : ''More cleanups and comments'',
		#parentIds : [
			''dac4bc3f29b260b69fc9b7b1a8230e63063ecc94''
		]
	},
	HiExampleCommit {
		#id : ''dac4bc3f29b260b69fc9b7b1a8230e63063ecc94'',
		#comment : ''Cleanups and comments'',
		#parentIds : [
			''0ed23fc6db548d1d6024c9bd316858f7dcee7980''
		]
	},
	HiExampleCommit {
		#id : ''0ed23fc6db548d1d6024c9bd316858f7dcee7980'',
		#comment : ''Organizing working copy and package state'',
		#parentIds : [
			''6a24350d6af3c719ec6047c7b0d2423d3dc7419c''
		]
	},
	HiExampleCommit {
		#id : ''6a24350d6af3c719ec6047c7b0d2423d3dc7419c'',
		#comment : ''Cleanup unused classes and methods'',
		#parentIds : [
			''c80622408b36edb7adefcbc76be51590cf90b8f0''
		]
	},
	HiExampleCommit {
		#id : ''c80622408b36edb7adefcbc76be51590cf90b8f0'',
		#comment : ''- Optimize merge of large repositories\n- Managing non fetched commits\n- Managing the absence of pull remotes'',
		#parentIds : [
			''46df3e98d41253333b0952b4bffa09f0e5f0776b''
		]
	},
	HiExampleCommit {
		#id : ''46df3e98d41253333b0952b4bffa09f0e5f0776b'',
		#comment : ''Optimizing loading of Pharo repository'',
		#parentIds : [
			''0f9698f6740a7620dc897d548342d3ffceb2844f''
		]
	},
	HiExampleCommit {
		#id : ''09bb31ab91bbb29cffe6ca68255774765f4d7704'',
		#comment : ''Merge pull request #559 from seandenigris/558-Gitlab-Private-Project-Support\n\n[Enh]: Fixes #558: Gitlab Private Project Support'',
		#parentIds : [
			''03c90f7a27ae62cb3028856c3bccf3a7b16f0312'',
			''6b88e52c3977940f6c78f2cb187118d7d4830fdc''
		]
	},
	HiExampleCommit {
		#id : ''03c90f7a27ae62cb3028856c3bccf3a7b16f0312'',
		#comment : ''Merge pull request #553 from elbow/elbow-patch-1\n\nAdd FAQ regarding need for SSH keys on Windows'',
		#parentIds : [
			''5f7a9750ab35ed3ed038a1b0f6f8f2ba09e56878'',
			''afa98c4ffc79b6018938a842cf2772ed8e44b705''
		]
	},
	HiExampleCommit {
		#id : ''6b88e52c3977940f6c78f2cb187118d7d4830fdc'',
		#comment : ''[Enh]: Fixes #558: Gitlab Private Project Support\n\nRequires pharo-project/pharo#20953: Gitlab Private Project Support (via Iceberg)'',
		#parentIds : [
			''f084a8f762fc1bd6975a4adc69b0d253dbf2dc54''
		]
	},
	HiExampleCommit {
		#id : ''0f9698f6740a7620dc897d548342d3ffceb2844f'',
		#comment : ''Removed IceTGitBranch\n\nPass on quick tests'',
		#parentIds : [
			''87897be3543a808b4e4b961a83b2eec432689605''
		]
	},
	HiExampleCommit {
		#id : ''87897be3543a808b4e4b961a83b2eec432689605'',
		#comment : ''adoptInstance: -> adoptCommit:'',
		#parentIds : [
			''b9b59efa8150137c6605220686ebeb54eefe1c64''
		]
	},
	HiExampleCommit {
		#id : ''b9b59efa8150137c6605220686ebeb54eefe1c64'',
		#comment : ''Pass on old iceberg tests'',
		#parentIds : [
			''4df71f53e934fe4a05a46d3b347fdef3527ed259''
		]
	},
	HiExampleCommit {
		#id : ''4df71f53e934fe4a05a46d3b347fdef3527ed259'',
		#comment : ''Preloading should unload everything\n'',
		#parentIds : [
			''fd6b12089e7b6170470a96d7860bae9e890f91d6''
		]
	},
	HiExampleCommit {
		#id : ''fd6b12089e7b6170470a96d7860bae9e890f91d6'',
		#comment : ''Only in pharo7 now\n'',
		#parentIds : [
			''4d83c108f020e08ee34492010708e4d9b2332945''
		]
	},
	HiExampleCommit {
		#id : ''4d83c108f020e08ee34492010708e4d9b2332945'',
		#comment : ''Fix metacello integration tests'',
		#parentIds : [
			''5990de25d2d6c16e308f1c6ba8f03d6a6840a5dd''
		]
	},
	HiExampleCommit {
		#id : ''5990de25d2d6c16e308f1c6ba8f03d6a6840a5dd'',
		#comment : ''Pass on original iceberg tests'',
		#parentIds : [
			''d5056356c2643ae180feea4dcde0652bd391d0f2''
		]
	},
	HiExampleCommit {
		#id : ''d5056356c2643ae180feea4dcde0652bd391d0f2'',
		#comment : ''Fixing wrong test'',
		#parentIds : [
			''cf61c4f5e017725ef80f10d1f6fac89ba25b517e''
		]
	},
	HiExampleCommit {
		#id : ''cf61c4f5e017725ef80f10d1f6fac89ba25b517e'',
		#comment : ''Fix unintentional creation of MC working copies'',
		#parentIds : [
			''d0fcf2dd859118d044f5cf134465ee2aee1265e9''
		]
	},
	HiExampleCommit {
		#id : ''d0fcf2dd859118d044f5cf134465ee2aee1265e9'',
		#comment : ''Implement adopt commit for fast-forward'',
		#parentIds : [
			''a7f9701975a3d7bc85b1f1554834e3977181ef6f''
		]
	},
	HiExampleCommit {
		#id : ''a7f9701975a3d7bc85b1f1554834e3977181ef6f'',
		#comment : ''Support for automatic merge fastforward and no conflict merge'',
		#parentIds : [
			''0a39c3a540e1bf1316bba9ef2f7b2bc4d7766040''
		]
	},
	HiExampleCommit {
		#id : ''0a39c3a540e1bf1316bba9ef2f7b2bc4d7766040'',
		#comment : ''Fix merges'',
		#parentIds : [
			''25bd3182598d12733921d3665a433d926ec47db4''
		]
	},
	HiExampleCommit {
		#id : ''25bd3182598d12733921d3665a433d926ec47db4'',
		#comment : ''Adding more tests'',
		#parentIds : [
			''4a8243c8a31c252b3a296e546a16203cf36e56c9''
		]
	},
	HiExampleCommit {
		#id : ''4a8243c8a31c252b3a296e546a16203cf36e56c9'',
		#comment : ''Calculating the state of the working copy from the state of the packages in it.\n'',
		#parentIds : [
			''2bdf16ab010b108b56a4b5deed207878c001f068''
		]
	},
	HiExampleCommit {
		#id : ''2bdf16ab010b108b56a4b5deed207878c001f068'',
		#comment : ''Merge'',
		#parentIds : [
			''c35649a3254e069273e21d204d58695a2c373d3e''
		]
	},
	HiExampleCommit {
		#id : ''c35649a3254e069273e21d204d58695a2c373d3e'',
		#comment : ''No need to check for rpackage existance. This method is only called on loaded packages.'',
		#parentIds : [
			''b69976573cc2b1f4175b439fa9200fd1cd33181e''
		]
	},
	HiExampleCommit {
		#id : ''b69976573cc2b1f4175b439fa9200fd1cd33181e'',
		#comment : ''Updating packages only for loaded packages'',
		#parentIds : [
			''ebf84f7ebdac2a26bff982648693421c3d994941''
		]
	},
	HiExampleCommit {
		#id : ''ebf84f7ebdac2a26bff982648693421c3d994941'',
		#comment : ''Better managing commits in the working copy'',
		#parentIds : [
			''2a17ad0197ee096b47733779786e2a9427f0726c''
		]
	},
	HiExampleCommit {
		#id : ''2a17ad0197ee096b47733779786e2a9427f0726c'',
		#comment : ''Added initial tests about working copy states'',
		#parentIds : [
			''da04aefc48bac12c1da713b7ec8600ef49b46de8''
		]
	},
	HiExampleCommit {
		#id : ''da04aefc48bac12c1da713b7ec8600ef49b46de8'',
		#comment : ''Tests are green'',
		#parentIds : [
			''96c9376b5aae45356081241d8d695f0da69c67f6''
		]
	},
	HiExampleCommit {
		#id : ''96c9376b5aae45356081241d8d695f0da69c67f6'',
		#comment : ''Fix 4 failing tests due to the creation of packages in working copy'',
		#parentIds : [
			''9f722e1718019ae04b3e79d684c8ccfe4d30fd60''
		]
	},
	HiExampleCommit {
		#id : ''9f722e1718019ae04b3e79d684c8ccfe4d30fd60'',
		#comment : ''Removing last fetched data.'',
		#parentIds : [
			''7dd855627de694ec0aab84cba4eb286c39ea23ea''
		]
	},
	HiExampleCommit {
		#id : ''7dd855627de694ec0aab84cba4eb286c39ea23ea'',
		#comment : ''Removing cache of incoming commits'',
		#parentIds : [
			''35246882d954a678436470987488f917bca767e9''
		]
	},
	HiExampleCommit {
		#id : ''35246882d954a678436470987488f917bca767e9'',
		#comment : ''Adding IceRepositoryModel.'',
		#parentIds : [
			''27dcc5baf1a499bf790c7b9ef52cc502c2a032e4''
		]
	},
	HiExampleCommit {
		#id : ''27dcc5baf1a499bf790c7b9ef52cc502c2a032e4'',
		#comment : ''Fixing reverts and getting commit in branch.'',
		#parentIds : [
			''3a4844a8224ac003dfc75947a25039f37624d8f2''
		]
	},
	HiExampleCommit {
		#id : ''3a4844a8224ac003dfc75947a25039f37624d8f2'',
		#comment : ''Adding cache for outgoing and incoming commits.\nThe cache is only invalidated on refresh.'',
		#parentIds : [
			''c3d567971a42ad94e72ec4656d092ef64560435a''
		]
	},
	HiExampleCommit {
		#id : ''c3d567971a42ad94e72ec4656d092ef64560435a'',
		#comment : ''Fixing version of libgit2.\nFixing the display string in remotes.'',
		#parentIds : [
			''386d0f9b85bb0ca4fd8e53e0590ec0c9ddcc4393''
		]
	},
	HiExampleCommit {
		#id : ''386d0f9b85bb0ca4fd8e53e0590ec0c9ddcc4393'',
		#comment : ''Fixing dependency to Ring2 and description in Metacello integration'',
		#parentIds : [
			''0ed4876a67c32015d074044bfcabded3cae32a74''
		]
	},
	HiExampleCommit {
		#id : ''0ed4876a67c32015d074044bfcabded3cae32a74'',
		#comment : ''Speed up tests using Ring2'',
		#parentIds : [
			''c6989f4098c9caf14d6c96ca59fd87a096e31073''
		]
	},
	HiExampleCommit {
		#id : ''c6989f4098c9caf14d6c96ca59fd87a096e31073'',
		#comment : ''Remove halt in commit'',
		#parentIds : [
			''e275384b342e6d138794f924c7b524210366db1b''
		]
	},
	HiExampleCommit {
		#id : ''e275384b342e6d138794f924c7b524210366db1b'',
		#comment : ''Experiments with Spec UI'',
		#parentIds : [
			''7ee963fe3c207f476c00555a78e7a881de006fbf''
		]
	},
	HiExampleCommit {
		#id : ''7ee963fe3c207f476c00555a78e7a881de006fbf'',
		#comment : ''Removed old classes'',
		#parentIds : [
			''4d030520df0fd60ad7925f765ad0b710c724227b''
		]
	},
	HiExampleCommit {
		#id : ''4d030520df0fd60ad7925f765ad0b710c724227b'',
		#comment : ''- Extending memory backend with examples\n- Added first Smart UI version'',
		#parentIds : [
			''2f5543b7b8c0cf8c0002d08751d73d8057ca61c4''
		]
	},
	HiExampleCommit {
		#id : ''2f5543b7b8c0cf8c0002d08751d73d8057ca61c4'',
		#comment : ''Rename old classes with Old prefix.\nRemoving *new* prefix from new classes.'',
		#parentIds : [
			''2ba824b62917f44143ddadbc828ab7cabed77e4e''
		]
	},
	HiExampleCommit {
		#id : ''2ba824b62917f44143ddadbc828ab7cabed77e4e'',
		#comment : ''Added new Smart UI statuses'',
		#parentIds : [
			''42bc9d790daf333cf381043979f45bdeb47fe3ff''
		]
	},
	HiExampleCommit {
		#id : ''42bc9d790daf333cf381043979f45bdeb47fe3ff'',
		#comment : ''Smart UI'',
		#parentIds : [
			''7d8d5da94551b83a0859128a6b09075f5a24be92''
		]
	},
	HiExampleCommit {
		#id : ''7d8d5da94551b83a0859128a6b09075f5a24be92'',
		#comment : ''Add Detached HEAD and Detached Working Copy states'',
		#parentIds : [
			''b378ad3f5183bd52ecfe94a914299597dd86fc83''
		]
	},
	HiExampleCommit {
		#id : ''b378ad3f5183bd52ecfe94a914299597dd86fc83'',
		#comment : ''Fix diff'',
		#parentIds : [
			''8f6c37d85a4a0580c59b92985fc6167523029210''
		]
	},
	HiExampleCommit {
		#id : ''8f6c37d85a4a0580c59b92985fc6167523029210'',
		#comment : ''Fix repository creation'',
		#parentIds : [
			''3578555e4ec604c9424d163769ab774324382d2a''
		]
	},
	HiExampleCommit {
		#id : ''3578555e4ec604c9424d163769ab774324382d2a'',
		#comment : ''Fix the full diff'',
		#parentIds : [
			''c886bb087a832d5edda1500c2008f68937782fbe''
		]
	},
	HiExampleCommit {
		#id : ''c886bb087a832d5edda1500c2008f68937782fbe'',
		#comment : ''Fix baseline to point to Iceberg-Memory'',
		#parentIds : [
			''d3cbe64e7e67071dd1c94d8b59e758edda71073a''
		]
	},
	HiExampleCommit {
		#id : ''d3cbe64e7e67071dd1c94d8b59e758edda71073a'',
		#comment : ''Added Iceberg-Memory for testing purposes'',
		#parentIds : [
			''1ae20d512feaa6e10d8290efda8f8a58f105b88a''
		]
	},
	HiExampleCommit {
		#id : ''1ae20d512feaa6e10d8290efda8f8a58f105b88a'',
		#comment : ''Enhance loaded version description for new repositories'',
		#parentIds : [
			''fd47402ab86f7a880f4c323decfcd2665b4a3446''
		]
	},
	HiExampleCommit {
		#id : ''fd47402ab86f7a880f4c323decfcd2665b4a3446'',
		#comment : ''Place halt  to capture cherry-picking'',
		#parentIds : [
			''dd8b69b5bb1587fdfa3d45d8b01b4c31a1132688''
		]
	},
	HiExampleCommit {
		#id : ''dd8b69b5bb1587fdfa3d45d8b01b4c31a1132688'',
		#comment : ''Fix commit comparison for iceberg UI'',
		#parentIds : [
			''d0b184e4a9f1eae7a22b3f968176a10acde4566c''
		]
	},
	HiExampleCommit {
		#id : ''d0b184e4a9f1eae7a22b3f968176a10acde4566c'',
		#comment : ''Changes needed to push'',
		#parentIds : [
			''c7ac1759e285844d3ac550f49048f004a5679cca''
		]
	},
	HiExampleCommit {
		#id : ''c7ac1759e285844d3ac550f49048f004a5679cca'',
		#comment : ''Heavy refactoring\n - added tests\n - multiple backends\n - review API and main class names'',
		#parentIds : [
			''5f7a9750ab35ed3ed038a1b0f6f8f2ba09e56878''
		]
	},
	HiExampleCommit {
		#id : ''afa98c4ffc79b6018938a842cf2772ed8e44b705'',
		#comment : ''Fix markup again'',
		#parentIds : [
			''3355fef8debc5cb10efe517136a794756cf25c40''
		]
	},
	HiExampleCommit {
		#id : ''3355fef8debc5cb10efe517136a794756cf25c40'',
		#comment : ''Correct markup'',
		#parentIds : [
			''9c1783808e98ff44e12d4b3eb925bb66b231b779''
		]
	},
	HiExampleCommit {
		#id : ''9c1783808e98ff44e12d4b3eb925bb66b231b779'',
		#comment : ''Add FAQ about the need for SSH keys on Windows'',
		#parentIds : [
			''f084a8f762fc1bd6975a4adc69b0d253dbf2dc54''
		]
	},
	HiExampleCommit {
		#id : ''f084a8f762fc1bd6975a4adc69b0d253dbf2dc54'',
		#comment : ''Merge pull request #551 from pharo-vcs/dev-0.6\n\nrelease 0.6.6'',
		#parentIds : [
			''c5765883fd0dc5b108e97eaf050172b4711a3285'',
			''5f7a9750ab35ed3ed038a1b0f6f8f2ba09e56878''
		]
	},
	HiExampleCommit {
		#id : ''5f7a9750ab35ed3ed038a1b0f6f8f2ba09e56878'',
		#comment : ''Merge pull request #538 from jecisc/cf_addTooltipToWorldMenuEntry\n\nAdd help to Iceberg world menu entry.'',
		#parentIds : [
			''4cb5b25ff0a3f2b8e39e37c9af30986beaa139a5'',
			''7851b4feb7d3ba1665a4449b07ea16845b79fabd''
		]
	},
	HiExampleCommit {
		#id : ''4cb5b25ff0a3f2b8e39e37c9af30986beaa139a5'',
		#comment : ''refactor to allow non interactive credential providers'',
		#parentIds : [
			''d24d8574a8f9c2504f51e8a699078df1789c359e''
		]
	},
	HiExampleCommit {
		#id : ''d24d8574a8f9c2504f51e8a699078df1789c359e'',
		#comment : ''mask dependency with KMRepository'',
		#parentIds : [
			''9c56a015e4da1c364e816caf0121b7444dbd328d''
		]
	},
	HiExampleCommit {
		#id : ''9c56a015e4da1c364e816caf0121b7444dbd328d'',
		#comment : ''mask dependency with SystemSettingsPersistence'',
		#parentIds : [
			''6aedbd007bb017794c9385d5496d80e038955e59''
		]
	},
	HiExampleCommit {
		#id : ''c5765883fd0dc5b108e97eaf050172b4711a3285'',
		#comment : ''update update script'',
		#parentIds : [
			''9ed497a77968a451cf49d3681fd4faca96f2e469''
		]
	},
	HiExampleCommit {
		#id : ''6aedbd007bb017794c9385d5496d80e038955e59'',
		#comment : ''Merge with refs/remotes/origin/dev-0.6'',
		#parentIds : [
			''37ebfdfb85536a4d8ced2076d640ae5911a366df'',
			''58c7a840a79d104860a930b7a08c2ada4f9720ba''
		]
	},
	HiExampleCommit {
		#id : ''7851b4feb7d3ba1665a4449b07ea16845b79fabd'',
		#comment : ''Add help to Iceberg world menu entry.'',
		#parentIds : [
			''01916385bdd53816d6025eda9cfbbb0cb6b26685''
		]
	},
	HiExampleCommit {
		#id : ''37ebfdfb85536a4d8ced2076d640ae5911a366df'',
		#comment : ''catch authentication error while fetching'',
		#parentIds : [
			''58c7a840a79d104860a930b7a08c2ada4f9720ba''
		]
	},
	HiExampleCommit {
		#id : ''9ed497a77968a451cf49d3681fd4faca96f2e469'',
		#comment : ''Merge pull request #535 from pharo-vcs/dev-0.6\n\nrelease 0.6.5'',
		#parentIds : [
			''518ef3cd08932a21bade385a41139fa89235e3d9'',
			''58c7a840a79d104860a930b7a08c2ada4f9720ba''
		]
	},
	HiExampleCommit {
		#id : ''58c7a840a79d104860a930b7a08c2ada4f9720ba'',
		#comment : ''update libgit to 1.0.2 and tonel to 1.0.5'',
		#parentIds : [
			''507b56fea674b7f07e64b202bb46bf73e388d80b''
		]
	},
	HiExampleCommit {
		#id : ''507b56fea674b7f07e64b202bb46bf73e388d80b'',
		#comment : ''extend the cache mechanism to make it work also for remotes (I DON\''T LIKE THIS, IS NOT ROBUST ENOUGH).\nfix a bug on pick sync repository'',
		#parentIds : [
			''24da57dac2f5fba4ba966221a7ab6663d8d3167a''
		]
	},
	HiExampleCommit {
		#id : ''24da57dac2f5fba4ba966221a7ab6663d8d3167a'',
		#comment : ''confirm sync before proceed.'',
		#parentIds : [
			''296b8d41bbdf2ec5264c3d99141b8840904161af''
		]
	},
	HiExampleCommit {
		#id : ''296b8d41bbdf2ec5264c3d99141b8840904161af'',
		#comment : ''change sync order'',
		#parentIds : [
			''92fbed1fa8810e5611f69c3f50de5485706aa2d8''
		]
	},
	HiExampleCommit {
		#id : ''92fbed1fa8810e5611f69c3f50de5485706aa2d8'',
		#comment : ''enhance error handling'',
		#parentIds : [
			''759d79be0378ba90dbd0e3700b2101b7bdfeab61''
		]
	},
	HiExampleCommit {
		#id : ''759d79be0378ba90dbd0e3700b2101b7bdfeab61'',
		#comment : ''add some authentication error checks'',
		#parentIds : [
			''341851291529ae48af380175ad5f88d7873ac13c''
		]
	},
	HiExampleCommit {
		#id : ''341851291529ae48af380175ad5f88d7873ac13c'',
		#comment : ''lookupCommit: needs to answer nil in case no commit is found (not LGit Error)'',
		#parentIds : [
			''b3742ede61d8aac4a03dd087918c43b9cb1d7e9e''
		]
	},
	HiExampleCommit {
		#id : ''b3742ede61d8aac4a03dd087918c43b9cb1d7e9e'',
		#comment : ''typo'',
		#parentIds : [
			''69f6abde12336e0e8d8c69a6debba914051c691e''
		]
	},
	HiExampleCommit {
		#id : ''69f6abde12336e0e8d8c69a6debba914051c691e'',
		#comment : ''oops, just macOS admits the -K parameter'',
		#parentIds : [
			''124eff90ec798085cc956db47f5683f75d7c25bc''
		]
	},
	HiExampleCommit {
		#id : ''124eff90ec798085cc956db47f5683f75d7c25bc'',
		#comment : ''add a default message to authentication errors to try to guide users to a solution.'',
		#parentIds : [
			''dfc9dd87586c4ce809345cbe723343b61f7a9a7c''
		]
	},
	HiExampleCommit {
		#id : ''dfc9dd87586c4ce809345cbe723343b61f7a9a7c'',
		#comment : ''add strategy to handle authetication errors (still a bit naked)'',
		#parentIds : [
			''fdbc9a9b3ee846867afcfa53a19aaadc0479d534''
		]
	},
	HiExampleCommit {
		#id : ''fdbc9a9b3ee846867afcfa53a19aaadc0479d534'',
		#comment : ''validate fetch'',
		#parentIds : [
			''efb022585a3a2c91fece36423e28f867cdc5ce61''
		]
	},
	HiExampleCommit {
		#id : ''efb022585a3a2c91fece36423e28f867cdc5ce61'',
		#comment : ''Merge with cc96f04bcb29fe328564c219a6eb308f8fed7f1a'',
		#parentIds : [
			''83d58434e40f44cc922800c4acaa93b4c87db085'',
			''cc96f04bcb29fe328564c219a6eb308f8fed7f1a''
		]
	},
	HiExampleCommit {
		#id : ''83d58434e40f44cc922800c4acaa93b4c87db085'',
		#comment : ''change actions order'',
		#parentIds : [
			''b3c51e4932b0d21bf89e974ad9d0d2ffa2eb6381''
		]
	},
	HiExampleCommit {
		#id : ''b3c51e4932b0d21bf89e974ad9d0d2ffa2eb6381'',
		#comment : ''- extracted pharo plugin actions to commands.\n- prepare for contribution is now "synchronize repositories"\n- create issue will start branch from current image commit '',
		#parentIds : [
			''0f14aeba364d1109375767ac4b7fd19c9187753c''
		]
	},
	HiExampleCommit {
		#id : ''cc96f04bcb29fe328564c219a6eb308f8fed7f1a'',
		#comment : ''Merge pull request #532 from akgrant43/20831-bad-gitlocal-reference\n\n20831 Bad gitlocal: reference causes infinite loop and out of memory'',
		#parentIds : [
			''0f14aeba364d1109375767ac4b7fd19c9187753c'',
			''5a7ba7c24c426adb05a3c9ec8b79cde94b758852''
		]
	},
	HiExampleCommit {
		#id : ''5a7ba7c24c426adb05a3c9ec8b79cde94b758852'',
		#comment : ''20831 Bad gitlocal: reference causes infinite loop and out of memory'',
		#parentIds : [
			''518ef3cd08932a21bade385a41139fa89235e3d9''
		]
	},
	HiExampleCommit {
		#id : ''0f14aeba364d1109375767ac4b7fd19c9187753c'',
		#comment : ''restore a better way of handlign errors (still lame, but no time to better refactors)'',
		#parentIds : [
			''c19efd8ae425fa4f0cf8a514ffeb87eebea32655''
		]
	},
	HiExampleCommit {
		#id : ''c19efd8ae425fa4f0cf8a514ffeb87eebea32655'',
		#comment : ''fix error'',
		#parentIds : [
			''8fa4c6b872b0b2c7dcf0a675e9b9763b7091de49''
		]
	},
	HiExampleCommit {
		#id : ''8fa4c6b872b0b2c7dcf0a675e9b9763b7091de49'',
		#comment : ''logging errors'',
		#parentIds : [
			''7536c9b4ea30ea9770e06ea3ef8c3fbb21cfc7d6''
		]
	},
	HiExampleCommit {
		#id : ''7536c9b4ea30ea9770e06ea3ef8c3fbb21cfc7d6'',
		#comment : ''add Pharo 7.0 to travis\n'',
		#parentIds : [
			''0ddc03899610a92d0e703da1e956af9405524ddf''
		]
	},
	HiExampleCommit {
		#id : ''0ddc03899610a92d0e703da1e956af9405524ddf'',
		#comment : ''should fix #493'',
		#parentIds : [
			''834e5e22b1a9bb15d936e8793c69fecbd6bab89a''
		]
	},
	HiExampleCommit {
		#id : ''834e5e22b1a9bb15d936e8793c69fecbd6bab89a'',
		#comment : ''fix #testPullNonFF'',
		#parentIds : [
			''9ef44317bd0af49af6fa111bde0e94d3efd8e88f''
		]
	},
	HiExampleCommit {
		#id : ''9ef44317bd0af49af6fa111bde0e94d3efd8e88f'',
		#comment : ''fix testMergeWithConflicts'',
		#parentIds : [
			''0d477baa2a20de1293df45ef53b56cf70386481b''
		]
	},
	HiExampleCommit {
		#id : ''0d477baa2a20de1293df45ef53b56cf70386481b'',
		#comment : ''fix #531'',
		#parentIds : [
			''1f62c15510ce90c01016e9be7495b64d198c5be1''
		]
	},
	HiExampleCommit {
		#id : ''518ef3cd08932a21bade385a41139fa89235e3d9'',
		#comment : ''Merge pull request #527 from pharo-vcs/dev-0.6\n\nfixes #526'',
		#parentIds : [
			''2310083e25906eed338c0cd66bf90b5f15a2443a'',
			''1f62c15510ce90c01016e9be7495b64d198c5be1''
		]
	},
	HiExampleCommit {
		#id : ''1f62c15510ce90c01016e9be7495b64d198c5be1'',
		#comment : ''fixes #526'',
		#parentIds : [
			''f9d5492b18c6231c4476263b9418015c51b378be''
		]
	},
	HiExampleCommit {
		#id : ''2310083e25906eed338c0cd66bf90b5f15a2443a'',
		#comment : ''Merge pull request #525 from pharo-vcs/dev-0.6\n\nrelease 0.6.4'',
		#parentIds : [
			''94c04792437f228fb727c3b4b2a0b6b031696d3d'',
			''f9d5492b18c6231c4476263b9418015c51b378be''
		]
	},
	HiExampleCommit {
		#id : ''f9d5492b18c6231c4476263b9418015c51b378be'',
		#comment : ''fixes #524'',
		#parentIds : [
			''a13871082e80e895d7c57b89f98606ae6a5b4230''
		]
	},
	HiExampleCommit {
		#id : ''a13871082e80e895d7c57b89f98606ae6a5b4230'',
		#comment : ''fixes #522'',
		#parentIds : [
			''c257904af5a46c26409c21b855840a0ef0aae238''
		]
	},
	HiExampleCommit {
		#id : ''c257904af5a46c26409c21b855840a0ef0aae238'',
		#comment : ''fixes #523'',
		#parentIds : [
			''1bb9bc26f9f0ead46a1db980f06ad7417dbe9558''
		]
	},
	HiExampleCommit {
		#id : ''1bb9bc26f9f0ead46a1db980f06ad7417dbe9558'',
		#comment : ''using libgit 1.0.1'',
		#parentIds : [
			''96bd1ba7b757dd4356aae5b908afa9c587874fdb''
		]
	},
	HiExampleCommit {
		#id : ''96bd1ba7b757dd4356aae5b908afa9c587874fdb'',
		#comment : ''metacello integration needs to be enabled to test '',
		#parentIds : [
			''f26ac86b3636e6f18334189153fa38b201e3aed4''
		]
	},
	HiExampleCommit {
		#id : ''f26ac86b3636e6f18334189153fa38b201e3aed4'',
		#comment : ''metacello integration should be restored to it\''s previous status, not always to true'',
		#parentIds : [
			''d202a5770b8e69d769e473fdbb28bfaea98c1c2c''
		]
	},
	HiExampleCommit {
		#id : ''d202a5770b8e69d769e473fdbb28bfaea98c1c2c'',
		#comment : ''fix test errors'',
		#parentIds : [
			''e207310956afb8ff6931293f72ea563953cfdcc4''
		]
	},
	HiExampleCommit {
		#id : ''e207310956afb8ff6931293f72ea563953cfdcc4'',
		#comment : ''skip failign test that should not be there.'',
		#parentIds : [
			''7b0da10661b3813bf33e57d319baa9cc2bde0211''
		]
	},
	HiExampleCommit {
		#id : ''7b0da10661b3813bf33e57d319baa9cc2bde0211'',
		#comment : ''claning iceberg completely to ensure is correctly loaded for test\n'',
		#parentIds : [
			''c0a52fbd5bb2343ef926844db1640db0a2390be3''
		]
	},
	HiExampleCommit {
		#id : ''c0a52fbd5bb2343ef926844db1640db0a2390be3'',
		#comment : ''Temporal remove of wrong tag'',
		#parentIds : [
			''ac18fec532eba4b7c5f8dd572411dc4d57d30766''
		]
	},
	HiExampleCommit {
		#id : ''ac18fec532eba4b7c5f8dd572411dc4d57d30766'',
		#comment : ''Merge branch \''dev-0.6\'' of github.com:pharo-vcs/iceberg into dev-0.6\n\n* \''dev-0.6\'' of github.com:pharo-vcs/iceberg:\n  use current metacello version [skip ci]\n'',
		#parentIds : [
			''d7f6a0cf196644d0123c0276cd66129bde496823'',
			''cc5e840f5ad91275ba932d2a89802c4b192c24ee''
		]
	},
	HiExampleCommit {
		#id : ''d7f6a0cf196644d0123c0276cd66129bde496823'',
		#comment : ''move test classes to their corresponding package\n'',
		#parentIds : [
			''9e260d4ebab0dea9737c90838ddcef22868313ef''
		]
	},
	HiExampleCommit {
		#id : ''cc5e840f5ad91275ba932d2a89802c4b192c24ee'',
		#comment : ''use current metacello version [skip ci]'',
		#parentIds : [
			''9e260d4ebab0dea9737c90838ddcef22868313ef''
		]
	},
	HiExampleCommit {
		#id : ''9e260d4ebab0dea9737c90838ddcef22868313ef'',
		#comment : ''removing baselines (to try to force an update)\n'',
		#parentIds : [
			''f0c323beda35708c5639e5ce53953c232316fae7''
		]
	},
	HiExampleCommit {
		#id : ''f0c323beda35708c5639e5ce53953c232316fae7'',
		#comment : ''using semver version of libgit2'',
		#parentIds : [
			''bc986b2a5b6b3f78799568f85df72046dfaf4f30''
		]
	},
	HiExampleCommit {
		#id : ''bc986b2a5b6b3f78799568f85df72046dfaf4f30'',
		#comment : ''categorise'',
		#parentIds : [
			''68f8f8ec27c1fbb85e3fc40d244248b78e60bdf1''
		]
	},
	HiExampleCommit {
		#id : ''68f8f8ec27c1fbb85e3fc40d244248b78e60bdf1'',
		#comment : ''add #get:parameters: convenience method'',
		#parentIds : [
			''1c6754c90fe977c759d98f369158f828601f4f66''
		]
	},
	HiExampleCommit {
		#id : ''49e29134cb68febf31b454e16a0818528e01df4c'',
		#comment : ''Merge pull request #519 from pharo-vcs/dev-0.6\n\nrelease 0.6.3'',
		#parentIds : [
			''56ec52d5f1efa82cee21120165de15ba708d89ee'',
			''1c6754c90fe977c759d98f369158f828601f4f66''
		]
	},
	HiExampleCommit {
		#id : ''1c6754c90fe977c759d98f369158f828601f4f66'',
		#comment : ''update to tonel 1.0.4'',
		#parentIds : [
			''816d487e96cb8bc7c96f1778bbac6d00e6471501''
		]
	},
	HiExampleCommit {
		#id : ''816d487e96cb8bc7c96f1778bbac6d00e6471501'',
		#comment : ''remove TODO.\n\ncloses #516'',
		#parentIds : [
			''0346f444c1cbf46918cb508699c89f19ec7daf03''
		]
	},
	HiExampleCommit {
		#id : ''0346f444c1cbf46918cb508699c89f19ec7daf03'',
		#comment : ''remove bad send to #name'',
		#parentIds : [
			''aecad04705a6aa7e75c909aa8a0a3e24114034b5''
		]
	},
	HiExampleCommit {
		#id : ''aecad04705a6aa7e75c909aa8a0a3e24114034b5'',
		#comment : ''add todo '',
		#parentIds : [
			''823d9c5a9a3993f79fd5abd9a2aee7ad4e788050''
		]
	},
	HiExampleCommit {
		#id : ''823d9c5a9a3993f79fd5abd9a2aee7ad4e788050'',
		#comment : ''validating push before pushing and before commit+push actions.\nthis should avoid the problem of creating tons of merges and weird cases when merging conflicts.'',
		#parentIds : [
			''00d5caa0f3377dff5e8d1bc84a7ea7aa0915fc3b''
		]
	},
	HiExampleCommit {
		#id : ''00d5caa0f3377dff5e8d1bc84a7ea7aa0915fc3b'',
		#comment : ''ensure we handle conflict error when checking out'',
		#parentIds : [
			''0a13f9f2ae626c7e2e0fa553c1900bd0fbfa0095''
		]
	},
	HiExampleCommit {
		#id : ''0a13f9f2ae626c7e2e0fa553c1900bd0fbfa0095'',
		#comment : ''catching merge abort exception'',
		#parentIds : [
			''6524fb65d22cefccdec49f00bafdb3ce506161da''
		]
	},
	HiExampleCommit {
		#id : ''6524fb65d22cefccdec49f00bafdb3ce506161da'',
		#comment : ''handling conflicts when merge.\nrefactores some methods'',
		#parentIds : [
			''eac70f9aca6c16663cae6483a00f352cc7732203''
		]
	},
	HiExampleCommit {
		#id : ''eac70f9aca6c16663cae6483a00f352cc7732203'',
		#comment : ''checkout the merged tree (otherwise is not in working copy and it will be marked for deletion)\ntranslate LGit_GIT_ENONFASTFORWARD to IceRemoteDesynchronized to catch the error as a reaction (not a preemption as before)'',
		#parentIds : [
			''4a72de32e08bb30e4f6dd90dc8794318bf542642''
		]
	},
	HiExampleCommit {
		#id : ''4a72de32e08bb30e4f6dd90dc8794318bf542642'',
		#comment : ''typo'',
		#parentIds : [
			''ab5b968ac836f622216c6f2871ee03168edfa617''
		]
	},
	HiExampleCommit {
		#id : ''ab5b968ac836f622216c6f2871ee03168edfa617'',
		#comment : ''validate push can be done before actually trying it.\nrefactor some code.\nremove some not used code.'',
		#parentIds : [
			''2f545955cdddfcc90215bbbbea82f3f855e5825b''
		]
	},
	HiExampleCommit {
		#id : ''2f545955cdddfcc90215bbbbea82f3f855e5825b'',
		#comment : ''move validatePullRequest to IceRepository.\ncreated a resumable error to correctly handle the issue.\n\ncloses #502'',
		#parentIds : [
			''70031e02c8d42b8777ac01b5470d185412b5ee29''
		]
	},
	HiExampleCommit {
		#id : ''70031e02c8d42b8777ac01b5470d185412b5ee29'',
		#comment : ''trying to get the right commit for versions when executing load/pull operations'',
		#parentIds : [
			''ff4e17778952a877c99fb09a5b3e088568a07c4c''
		]
	},
	HiExampleCommit {
		#id : ''ff4e17778952a877c99fb09a5b3e088568a07c4c'',
		#comment : ''Merge pull request #514 from noha/dev-0.6\n\nEven better detect of possible referenceCommit'',
		#parentIds : [
			''bce4d93332127a79071ae407d4dbaad405d110e2'',
			''b12ae9bec0b314530bd328e68c52b071974c242e''
		]
	},
	HiExampleCommit {
		#id : ''b12ae9bec0b314530bd328e68c52b071974c242e'',
		#comment : ''After we sorted the anchestry we need to take the first element instead of anyOne'',
		#parentIds : [
			''0d76da1aa1cc63d5fa8b08a4ff401f271b2a9132''
		]
	},
	HiExampleCommit {
		#id : ''bce4d93332127a79071ae407d4dbaad405d110e2'',
		#comment : ''Merge pull request #512 from jecisc/cf_comment_iceberg_packages\n\ncf_comment_iceberg_packages'',
		#parentIds : [
			''8664112eef21f75c7a51c7dce010cf1cbe6eee4a'',
			''c2e725f8713fe7cd567a287e320ae593d537d117''
		]
	},
	HiExampleCommit {
		#id : ''c2e725f8713fe7cd567a287e320ae593d537d117'',
		#comment : ''Add packages comments to Iceberg. \n\nAll pharo packages should have a minimum of comments. \n\nClose issue #511'',
		#parentIds : [
			''56ec52d5f1efa82cee21120165de15ba708d89ee''
		]
	},
	HiExampleCommit {
		#id : ''0d76da1aa1cc63d5fa8b08a4ff401f271b2a9132'',
		#comment : ''Trying to do more in order to detect the best version. If there are more versions in the image we can still try to find the most actual one by looking at the anchestry. If the anchestry chain is consistent than we get the actual one'',
		#parentIds : [
			''7b3c9f86398edda54b9e00fc148d1995eb358f89''
		]
	},
	HiExampleCommit {
		#id : ''8664112eef21f75c7a51c7dce010cf1cbe6eee4a'',
		#comment : ''Merge pull request #509 from noha/dev-0.6\n\nMove reference commit to head if no other commit between saved package commit and head commit changes a package. '',
		#parentIds : [
			''fc9c3e0bb8e27f3a8a590a553e363b0c1398202c'',
			''7b3c9f86398edda54b9e00fc148d1995eb358f89''
		]
	},
	HiExampleCommit {
		#id : ''7b3c9f86398edda54b9e00fc148d1995eb358f89'',
		#comment : ''Move reference commit to head if no other commit between saved package commit and head commit changes a package'',
		#parentIds : [
			''fc9c3e0bb8e27f3a8a590a553e363b0c1398202c''
		]
	},
	HiExampleCommit {
		#id : ''fc9c3e0bb8e27f3a8a590a553e363b0c1398202c'',
		#comment : ''closes #507'',
		#parentIds : [
			''f7a665245f7ae5507b7209b2b926bf2fb59f0a7f''
		]
	},
	HiExampleCommit {
		#id : ''f7a665245f7ae5507b7209b2b926bf2fb59f0a7f'',
		#comment : ''Merge pull request #506 from pharo-vcs/verify-sync-before-commiting\n\nVerify sync before commiting'',
		#parentIds : [
			''c1f465effe88042de4cc8ae5aadb4c40ee4f00aa'',
			''8b32e0459ada4e7a815ed2d0d8a36f277c4ce3f4''
		]
	},
	HiExampleCommit {
		#id : ''8b32e0459ada4e7a815ed2d0d8a36f277c4ce3f4'',
		#comment : ''IceMergeWorkingCopyCommand working.\nIceCommand renamed to IceRepositoryCommand.\n\nnow, I discovered we do not have any feedback if we cancel a merge, and the tool acts as if the merge happened anyway. This is a problem that exceeds iceberg, but is annoying and needs to be fixed.\n'',
		#parentIds : [
			''4b2e620a5907a8df864c0ba6618e2b0edbe52b53''
		]
	},
	HiExampleCommit {
		#id : ''4b2e620a5907a8df864c0ba6618e2b0edbe52b53'',
		#comment : ''first approach (branch then commit) working. \nstill a lot of test to go.'',
		#parentIds : [
			''c1f465effe88042de4cc8ae5aadb4c40ee4f00aa''
		]
	},
	HiExampleCommit {
		#id : ''c1f465effe88042de4cc8ae5aadb4c40ee4f00aa'',
		#comment : ''Merge with 4e0614e9cd1232d1b634e8b2d394f8f78df1af0a'',
		#parentIds : [
			''b172337dedf21b1ed0d64cdd03779d3637084a26'',
			''4e0614e9cd1232d1b634e8b2d394f8f78df1af0a''
		]
	},
	HiExampleCommit {
		#id : ''4e0614e9cd1232d1b634e8b2d394f8f78df1af0a'',
		#comment : ''Merge pull request #500 from tesonep/fixing-version-of-tonel-for-releases\n\nFixing version of Tonel'',
		#parentIds : [
			''5b711579f44058a11432216c1a9815a6068504bb'',
			''8b52d697b77749afdc3954aed9ccdcc87aa395b0''
		]
	},
	HiExampleCommit {
		#id : ''8b52d697b77749afdc3954aed9ccdcc87aa395b0'',
		#comment : ''The version of Tonel should be fixed, if it is not the pharo bootstrap is unstable and unrepetible\n'',
		#parentIds : [
			''5b711579f44058a11432216c1a9815a6068504bb''
		]
	},
	HiExampleCommit {
		#id : ''b172337dedf21b1ed0d64cdd03779d3637084a26'',
		#comment : ''close #497'',
		#parentIds : [
			''5b711579f44058a11432216c1a9815a6068504bb''
		]
	},
	HiExampleCommit {
		#id : ''5b711579f44058a11432216c1a9815a6068504bb'',
		#comment : ''closes #496'',
		#parentIds : [
			''b450fa51ef11ec2d3c2b2de0929d1ce21e515b3a''
		]
	},
	HiExampleCommit {
		#id : ''56ec52d5f1efa82cee21120165de15ba708d89ee'',
		#comment : ''Merge pull request #492 from pharo-vcs/dev-0.6\n\nfix tests that fails on windows'',
		#parentIds : [
			''073562da99dd99b4a4e9973df2325b2f35449e3f'',
			''b450fa51ef11ec2d3c2b2de0929d1ce21e515b3a''
		]
	},
	HiExampleCommit {
		#id : ''b450fa51ef11ec2d3c2b2de0929d1ce21e515b3a'',
		#comment : ''fix tests that fails on windows.\n'',
		#parentIds : [
			''a524a0a0471ca3714ef15accb322a3f2d7f5eb0a''
		]
	},
	HiExampleCommit {
		#id : ''073562da99dd99b4a4e9973df2325b2f35449e3f'',
		#comment : ''Merge pull request #491 from pharo-vcs/dev-0.6\n\nrelease 0.6.2'',
		#parentIds : [
			''94c04792437f228fb727c3b4b2a0b6b031696d3d'',
			''a524a0a0471ca3714ef15accb322a3f2d7f5eb0a''
		]
	},
	HiExampleCommit {
		#id : ''a524a0a0471ca3714ef15accb322a3f2d7f5eb0a'',
		#comment : ''Merge with 4edc4bce58c06ceb390c4ef0d75173560e87fa53'',
		#parentIds : [
			''a9f2b37f141ef4b100598c7bf666771939c8eb4f'',
			''4edc4bce58c06ceb390c4ef0d75173560e87fa53''
		]
	},
	HiExampleCommit {
		#id : ''a9f2b37f141ef4b100598c7bf666771939c8eb4f'',
		#comment : ''dialog to redirect the attempt to browse an iceberg package with monticello.\n\ncloses #490'',
		#parentIds : [
			''b6443d0ab5be312be297c9b73ad9f2db38664de4''
		]
	},
	HiExampleCommit {
		#id : ''b6443d0ab5be312be297c9b73ad9f2db38664de4'',
		#comment : ''comparing url as case insensitive'',
		#parentIds : [
			''a31faa588295646ac69b16accdc087477444fb7f''
		]
	},
	HiExampleCommit {
		#id : ''4edc4bce58c06ceb390c4ef0d75173560e87fa53'',
		#comment : ''move FAQ to the top'',
		#parentIds : [
			''f6019965e6e825b3eb171e087293f90150a9bf8a''
		]
	},
	HiExampleCommit {
		#id : ''f6019965e6e825b3eb171e087293f90150a9bf8a'',
		#comment : ''add a FAQ'',
		#parentIds : [
			''a31faa588295646ac69b16accdc087477444fb7f''
		]
	},
	HiExampleCommit {
		#id : ''a31faa588295646ac69b16accdc087477444fb7f'',
		#comment : ''add dash to optional characters'',
		#parentIds : [
			''31f022dfbd5fcadb46e1fdcd3bcb0850de1ba65f''
		]
	},
	HiExampleCommit {
		#id : ''31f022dfbd5fcadb46e1fdcd3bcb0850de1ba65f'',
		#comment : ''replace cypress with filetree'',
		#parentIds : [
			''71aef4eeca2c6e8bd7b444da6aa2c5dc3cfa551f''
		]
	},
	HiExampleCommit {
		#id : ''71aef4eeca2c6e8bd7b444da6aa2c5dc3cfa551f'',
		#comment : ''the proper id of the filetree reader is "filetree"'',
		#parentIds : [
			''6d9b8ae9b9cfa8afe6f999a3b7f7078fde1c329b''
		]
	},
	HiExampleCommit {
		#id : ''6d9b8ae9b9cfa8afe6f999a3b7f7078fde1c329b'',
		#comment : ''dix a problem when moving packages from non-iceberg to icebeg repositories. '',
		#parentIds : [
			''34be8c63e89b4a8e6bb6a95dea6b7f8b2e0663c9''
		]
	},
	HiExampleCommit {
		#id : ''34be8c63e89b4a8e6bb6a95dea6b7f8b2e0663c9'',
		#comment : ''Merge pull request #485 from noha/dev-0.6\n\nCorrect usage of bitbucket typed repository'',
		#parentIds : [
			''9fdfbaf2d83cdf2be221453a5daf8802576c8d21'',
			''645b0059104959d3c27e1f56a5ecd9b69259896d''
		]
	},
	HiExampleCommit {
		#id : ''645b0059104959d3c27e1f56a5ecd9b69259896d'',
		#comment : ''Moved getOrCreateIcebergRepository up to MCGitBasedNetworkRepository'',
		#parentIds : [
			''7901f753681fab6377c3fc30916beaf0e6437881''
		]
	},
	HiExampleCommit {
		#id : ''7901f753681fab6377c3fc30916beaf0e6437881'',
		#comment : ''Merge with refs/remotes/upstream/dev-0.6'',
		#parentIds : [
			''f51220a1a3b4de039f6debdb497618dab369cdf4'',
			''9fdfbaf2d83cdf2be221453a5daf8802576c8d21''
		]
	},
	HiExampleCommit {
		#id : ''f51220a1a3b4de039f6debdb497618dab369cdf4'',
		#comment : ''Merge branch \''dev-0.6\'' of github.com:pharo-vcs/iceberg into dev-0.6\n'',
		#parentIds : [
			''d85f8faf9908b3b99f0442b1548bfa3989a70907'',
			''9fdfbaf2d83cdf2be221453a5daf8802576c8d21''
		]
	},
	HiExampleCommit {
		#id : ''9fdfbaf2d83cdf2be221453a5daf8802576c8d21'',
		#comment : ''fix version of tonel (not good to use "master")'',
		#parentIds : [
			''94c04792437f228fb727c3b4b2a0b6b031696d3d''
		]
	},
	HiExampleCommit {
		#id : ''d85f8faf9908b3b99f0442b1548bfa3989a70907'',
		#comment : ''Iceberg repository should return MCBitbucketRepository instead of MCGithubRepository'',
		#parentIds : [
			''94c04792437f228fb727c3b4b2a0b6b031696d3d''
		]
	},
	HiExampleCommit {
		#id : ''94c04792437f228fb727c3b4b2a0b6b031696d3d'',
		#comment : ''Merge pull request #473 from zecke/url-handling\n\nUrl handling'',
		#parentIds : [
			''f061625adc43cd5cb9b03993e6ff186ae34ec7f9'',
			''95226439e416c28e8496e958cf73449cb88e6b11''
		]
	},
	HiExampleCommit {
		#id : ''f061625adc43cd5cb9b03993e6ff186ae34ec7f9'',
		#comment : ''Merge pull request #480 from pharo-vcs/dev-0.6\n\nrelease 0.6.1'',
		#parentIds : [
			''01916385bdd53816d6025eda9cfbbb0cb6b26685'',
			''bfd3831082c1b3e5dadfd05688d3d096290fe58c''
		]
	},
	HiExampleCommit {
		#id : ''bfd3831082c1b3e5dadfd05688d3d096290fe58c'',
		#comment : ''Merge with 1bc3f6264c9ef60979c33506a056c7a23d35ff9c'',
		#parentIds : [
			''db0f3f59a8b3b20f6e9c8438e52dfe18315dd5a1'',
			''1bc3f6264c9ef60979c33506a056c7a23d35ff9c''
		]
	},
	HiExampleCommit {
		#id : ''db0f3f59a8b3b20f6e9c8438e52dfe18315dd5a1'',
		#comment : ''ensure removed packages are taken modified (to have them on list) '',
		#parentIds : [
			''465c2fe9a1b92bd7a186423e633ef136d878b7a8''
		]
	},
	HiExampleCommit {
		#id : ''1bc3f6264c9ef60979c33506a056c7a23d35ff9c'',
		#comment : ''Merge pull request #478 from jecisc/cf_load_Pharo6_package_in_pharo6\n\nPharo 6 specific package was not loaded into Pharo 6. '',
		#parentIds : [
			''465c2fe9a1b92bd7a186423e633ef136d878b7a8'',
			''13968c4f378dfd434efb90d844772776dcb5e8b0''
		]
	},
	HiExampleCommit {
		#id : ''13968c4f378dfd434efb90d844772776dcb5e8b0'',
		#comment : ''Pharo 6 specific package was not loaded into Pharo 6. I added a required on Iceberg-UI to ensure the loading. \n\nClose issue https://github.com/pharo-vcs/iceberg/issues/477'',
		#parentIds : [
			''01916385bdd53816d6025eda9cfbbb0cb6b26685''
		]
	},
	HiExampleCommit {
		#id : ''465c2fe9a1b92bd7a186423e633ef136d878b7a8'',
		#comment : ''fix typo'',
		#parentIds : [
			''01916385bdd53816d6025eda9cfbbb0cb6b26685''
		]
	},
	HiExampleCommit {
		#id : ''01916385bdd53816d6025eda9cfbbb0cb6b26685'',
		#comment : ''reorder defaults'',
		#parentIds : [
			''2e2ca520ffb040fc52a7229408faa641bb35d213''
		]
	},
	HiExampleCommit {
		#id : ''2e2ca520ffb040fc52a7229408faa641bb35d213'',
		#comment : ''add metacello update'',
		#parentIds : [
			''0d1bd38eb0955488f8d23b1feb7bc2efd182c1ec''
		]
	},
	HiExampleCommit {
		#id : ''0d1bd38eb0955488f8d23b1feb7bc2efd182c1ec'',
		#comment : ''Merge pull request #476 from pharo-vcs/dev-0.6\n\ndefault code directory is empty for the moment'',
		#parentIds : [
			''1af9f820735adacef6e614e833825b8b65ae0ffc'',
			''125fecbbcc9925a9cc9536f86c151bfd878cf3af''
		]
	},
	HiExampleCommit {
		#id : ''125fecbbcc9925a9cc9536f86c151bfd878cf3af'',
		#comment : ''default code directory is empty for the moment'',
		#parentIds : [
			''95e57490f64d643005c16acbfc37230be18c48b5''
		]
	},
	HiExampleCommit {
		#id : ''1af9f820735adacef6e614e833825b8b65ae0ffc'',
		#comment : ''Merge pull request #475 from pharo-vcs/dev-0.6\n\nrelease 0.6'',
		#parentIds : [
			''fef1f906874542ea64c666404084d3f900559fe3'',
			''95e57490f64d643005c16acbfc37230be18c48b5''
		]
	},
	HiExampleCommit {
		#id : ''95e57490f64d643005c16acbfc37230be18c48b5'',
		#comment : ''update README [skip-ci]\n'',
		#parentIds : [
			''4ecdd4f451e347475277d085771b7c4e58e612cf''
		]
	},
	HiExampleCommit {
		#id : ''4ecdd4f451e347475277d085771b7c4e58e612cf'',
		#comment : ''clean baseline'',
		#parentIds : [
			''b5422238aa64ed9a8b08de91f718ba7f6c80032b''
		]
	},
	HiExampleCommit {
		#id : ''b5422238aa64ed9a8b08de91f718ba7f6c80032b'',
		#comment : ''fix typo on LibGit name'',
		#parentIds : [
			''103068fa17807ac8f7763b837ffafea309662861''
		]
	},
	HiExampleCommit {
		#id : ''103068fa17807ac8f7763b837ffafea309662861'',
		#comment : ''remove pre-remove of packages (looks like metacello does not like that)'',
		#parentIds : [
			''a146c09f0d5413803f0c81556636e962dfc7ec0b''
		]
	},
	HiExampleCommit {
		#id : ''a146c09f0d5413803f0c81556636e962dfc7ec0b'',
		#comment : ''remove an incorrect dependency'',
		#parentIds : [
			''85001775448b67f99e165651000a3fa3ff59d05c''
		]
	},
	HiExampleCommit {
		#id : ''85001775448b67f99e165651000a3fa3ff59d05c'',
		#comment : ''and remove again :('',
		#parentIds : [
			''c3331d9ecdf2fec2e0f8226a7ff82d63b9fd622e''
		]
	},
	HiExampleCommit {
		#id : ''c3331d9ecdf2fec2e0f8226a7ff82d63b9fd622e'',
		#comment : ''restore'',
		#parentIds : [
			''88eb3c6a3d65c074439525fb7b530dd3a5933777''
		]
	},
	HiExampleCommit {
		#id : ''88eb3c6a3d65c074439525fb7b530dd3a5933777'',
		#comment : ''remove pre-load'',
		#parentIds : [
			''3a988d0a63b15ed30b1d90ffc28c6681c72fd14e''
		]
	},
	HiExampleCommit {
		#id : ''3a988d0a63b15ed30b1d90ffc28c6681c72fd14e'',
		#comment : ''disable unload of packages'',
		#parentIds : [
			''ea00232eb1371182428af69e3b73470ebc052189''
		]
	},
	HiExampleCommit {
		#id : ''ea00232eb1371182428af69e3b73470ebc052189'',
		#comment : ''cleaning a bit'',
		#parentIds : [
			''ef4a03c2d191e6e36a32fe50318da1b0d94ffee1''
		]
	},
	HiExampleCommit {
		#id : ''ef4a03c2d191e6e36a32fe50318da1b0d94ffee1'',
		#comment : ''try an atomic loading project'',
		#parentIds : [
			''10317f29e553ca159f99b50867421cc5df766ced''
		]
	},
	HiExampleCommit {
		#id : ''10317f29e553ca159f99b50867421cc5df766ced'',
		#comment : ''add extra dependencies'',
		#parentIds : [
			''2cce8cbb3e4937ad0da5260fcc281a4774d35c5e''
		]
	},
	HiExampleCommit {
		#id : ''2cce8cbb3e4937ad0da5260fcc281a4774d35c5e'',
		#comment : ''and also no BaselineOfLibGit'',
		#parentIds : [
			''e7f6fc52b58547150e33c3d5e29338650930c42e''
		]
	},
	HiExampleCommit {
		#id : ''e7f6fc52b58547150e33c3d5e29338650930c42e'',
		#comment : ''I guess is not the best to auto remove your self.'',
		#parentIds : [
			''fbaeb6716161f2ace5004d17e8824a2abf524176''
		]
	},
	HiExampleCommit {
		#id : ''fbaeb6716161f2ace5004d17e8824a2abf524176'',
		#comment : ''add a pre-load on baseline for Pharo6'',
		#parentIds : [
			''5bddecbf286d44377f2788b8f7aa84870fde081e''
		]
	},
	HiExampleCommit {
		#id : ''5bddecbf286d44377f2788b8f7aa84870fde081e'',
		#comment : ''remove gitfiletree from baseline'',
		#parentIds : [
			''ac07f892e7cfeaaad6dcfa6b29c2c55c40d18d91''
		]
	},
	HiExampleCommit {
		#id : ''ac07f892e7cfeaaad6dcfa6b29c2c55c40d18d91'',
		#comment : ''trying fixing pharo version and using latest metacello\n'',
		#parentIds : [
			''ec7a7bda0c8a362fe8f5e08b50d492373db3f439''
		]
	},
	HiExampleCommit {
		#id : ''ec7a7bda0c8a362fe8f5e08b50d492373db3f439'',
		#comment : ''Merge branch \''dev-0.6\'' of github.com:pharo-vcs/iceberg into dev-0.6\n\n* \''dev-0.6\'' of github.com:pharo-vcs/iceberg:\n  $/ in a branch name is common. Pass indication of isRemote down\n'',
		#parentIds : [
			''0c782cb7a960db45df063c8c7e7d4931b72429a1'',
			''ddd700c4de2cc7d3ae41c5b7123e7119d45b142e''
		]
	},
	HiExampleCommit {
		#id : ''0c782cb7a960db45df063c8c7e7d4931b72429a1'',
		#comment : ''update readme\n'',
		#parentIds : [
			''fb186a1dbbb10ba80ba83d8e49f4479784a6523e''
		]
	},
	HiExampleCommit {
		#id : ''fb186a1dbbb10ba80ba83d8e49f4479784a6523e'',
		#comment : ''remove unused packages\n'',
		#parentIds : [
			''b02e9f4d6a527ee7ba3038aa61a95cd9ddc0591c''
		]
	},
	HiExampleCommit {
		#id : ''ddd700c4de2cc7d3ae41c5b7123e7119d45b142e'',
		#comment : ''Merge pull request #474 from zecke/fix-branch-names\n\n$/ in a branch name is common. Pass indication of isRemote down'',
		#parentIds : [
			''b02e9f4d6a527ee7ba3038aa61a95cd9ddc0591c'',
			''3e3759ed91ae772a5408ed7951d032ee826e4101''
		]
	},
	HiExampleCommit {
		#id : ''b02e9f4d6a527ee7ba3038aa61a95cd9ddc0591c'',
		#comment : ''temporary remove of dependency with Metacello for Pharo6.1 because it will break loading on Pharo7.0'',
		#parentIds : [
			''38a7e327ed89a011fa2628aeecffcb4c67bfd5ad''
		]
	},
	HiExampleCommit {
		#id : ''38a7e327ed89a011fa2628aeecffcb4c67bfd5ad'',
		#comment : ''make tonel format by default, but separate current default from legacy default (the format that is used in case no specific format is found) '',
		#parentIds : [
			''97fc73a7113f7a42b3d1821a75e70e5a39883dd2''
		]
	},
	HiExampleCommit {
		#id : ''3e3759ed91ae772a5408ed7951d032ee826e4101'',
		#comment : ''$/ in a branch name is common. Pass indication of isRemote down\n\nI use some semantics in branch names. E.g. fixes/proj-number or \nfeature/feature-name.  Or shared projects use user/their-work so make\nit work. Pass name instead of baseName to get the full branch (the\nrefs/heads/ will be removed by other code) and pass the indication of\nlocal/remote down to libgit.'',
		#parentIds : [
			''fef1f906874542ea64c666404084d3f900559fe3''
		]
	},
	HiExampleCommit {
		#id : ''95226439e416c28e8496e958cf73449cb88e6b11'',
		#comment : ''Implement git:// urls for Iceberg\n\nSupport git:// for non authenticated fetches from remote repos. Adjust\nthe IceScpRemote>>#canHandleUrl: so it doesn\''t match anything that\nlooks like a URI.'',
		#parentIds : [
			''0c21214696b9deabfa9a9b3e30a38d624f7a64fd''
		]
	},
	HiExampleCommit {
		#id : ''0c21214696b9deabfa9a9b3e30a38d624f7a64fd'',
		#comment : ''Use ZnUrl to parse HTTP URL\n\nUse ZnUrl to parse the url. Add tests.  The notion of owner/project is\nwrong as I can have git repos in many other places. E.g. in one of my\nprojects it is  http://git.osmocom.org/libosmo-sccp. Leave that for now.\npath seems to be an empty ordered collection so do not write it.\n\nImproves: #457'',
		#parentIds : [
			''af294ab10c14af4d7a8abee6b46ed64266e6f18c''
		]
	},
	HiExampleCommit {
		#id : ''af294ab10c14af4d7a8abee6b46ed64266e6f18c'',
		#comment : ''Parse ssh URLs with port and different usernames\n\nThis can parse git@host:port/owner/repo and user@host:owner/repo name. I wanted to use ZnUrl but git is not using an absoluteURI. So\n\ngit@github.com:pharo-vcs/iceberg could not be parsed and I stay with \nthe regexp. The notion of owner/project is not universally true either\nbut that is left for a future commit.'',
		#parentIds : [
			''c826e3e9818253c810c88da4a250245dea38e4c9''
		]
	},
	HiExampleCommit {
		#id : ''c826e3e9818253c810c88da4a250245dea38e4c9'',
		#comment : ''Handle absolute directory paths as file as well\n\ngit clone some-repo new-repo will create a\n\n[remote "origin"]\n\turl = /Users/pharo/foo/\n\tfetch = +refs/heads/*:refs/remotes/origin/*\n\nAdd test cases.\n'',
		#parentIds : [
			''fef1f906874542ea64c666404084d3f900559fe3''
		]
	},
	HiExampleCommit {
		#id : ''97fc73a7113f7a42b3d1821a75e70e5a39883dd2'',
		#comment : ''add convenience constructor'',
		#parentIds : [
			''2f488f249e7898fc8248f6b209a13e178f231c31''
		]
	},
	HiExampleCommit {
		#id : ''2f488f249e7898fc8248f6b209a13e178f231c31'',
		#comment : ''add creator with stub'',
		#parentIds : [
			''8082ffe8441ef57e9877ca2090784d0bfc931e0c''
		]
	},
	HiExampleCommit {
		#id : ''8082ffe8441ef57e9877ca2090784d0bfc931e0c'',
		#comment : ''turns out it was a problem with the tags, not with the update process'',
		#parentIds : [
			''43680a97eaca81f01a8dba97fb0a40fa7d88ddda''
		]
	},
	HiExampleCommit {
		#id : ''43680a97eaca81f01a8dba97fb0a40fa7d88ddda'',
		#comment : ''looks like metacello needs to be updated manually :('',
		#parentIds : [
			''43852fa69e4a5e7d37097229639fd345aced976c''
		]
	},
	HiExampleCommit {
		#id : ''43852fa69e4a5e7d37097229639fd345aced976c'',
		#comment : ''add loads'',
		#parentIds : [
			''42da0a6f9720d121fba5a31d45e6c41177c5e97d''
		]
	},
	HiExampleCommit {
		#id : ''42da0a6f9720d121fba5a31d45e6c41177c5e97d'',
		#comment : ''revert'',
		#parentIds : [
			''074ea83ae480865e9c2b9a227906c61075036cc1''
		]
	},
	HiExampleCommit {
		#id : ''074ea83ae480865e9c2b9a227906c61075036cc1'',
		#comment : ''try to fix baseline for Pharo6.1'',
		#parentIds : [
			''8fe3b5907f17a491e87f0e3601d08e8e9c5d74bd''
		]
	},
	HiExampleCommit {
		#id : ''8fe3b5907f17a491e87f0e3601d08e8e9c5d74bd'',
		#comment : ''added new metacello version for Pharo6.1 '',
		#parentIds : [
			''89d7638cfc75a1a511aeae2391ab810d4afcf3c9''
		]
	},
	HiExampleCommit {
		#id : ''89d7638cfc75a1a511aeae2391ab810d4afcf3c9'',
		#comment : ''Merge with 4f3dbc7a24630bd63bc355c0539844a201e20474'',
		#parentIds : [
			''34834db2f37b6e5f1cb0ac011081a8e26a893ebd'',
			''4f3dbc7a24630bd63bc355c0539844a201e20474''
		]
	},
	HiExampleCommit {
		#id : ''34834db2f37b6e5f1cb0ac011081a8e26a893ebd'',
		#comment : ''if subdirectory is empty git will ignore it.  \nWe need to process the absense.\n\nfix #464'',
		#parentIds : [
			''5aee13467a54d36d78981a63749c8065f736f7e2''
		]
	},
	HiExampleCommit {
		#id : ''4f3dbc7a24630bd63bc355c0539844a201e20474'',
		#comment : ''Merge pull request #472 from seandenigris/471-setting-for-default-code-subfolder\n\n[ENH]: Fix #471 - Setting for Default Code Subdirectory'',
		#parentIds : [
			''5aee13467a54d36d78981a63749c8065f736f7e2'',
			''512bb692994f781cebf3741c3890576aaf263b4c''
		]
	},
	HiExampleCommit {
		#id : ''512bb692994f781cebf3741c3890576aaf263b4c'',
		#comment : ''Merge branch \''dev-0.6\'' into 471-setting-for-default-code-subfolder'',
		#parentIds : [
			''49b89cd6b529a27453cbb52d2962861f26b4ac01'',
			''5aee13467a54d36d78981a63749c8065f736f7e2''
		]
	},
	HiExampleCommit {
		#id : ''5aee13467a54d36d78981a63749c8065f736f7e2'',
		#comment : ''add mutator to stubVersionInfo, because I may want to add a stub with a decent name/date/etc.'',
		#parentIds : [
			''359210a1c586514d3d37f379a1dddb9e0574845a''
		]
	},
	HiExampleCommit {
		#id : ''359210a1c586514d3d37f379a1dddb9e0574845a'',
		#comment : ''adding a proxy for version info of packages belonging to not-present repositories.'',
		#parentIds : [
			''1966bf7cc83b9f2a37b46eb4f06417326e02bb8a''
		]
	},
	HiExampleCommit {
		#id : ''1966bf7cc83b9f2a37b46eb4f06417326e02bb8a'',
		#comment : ''managers do not need to be cached'',
		#parentIds : [
			''5a01697b959969704c6b40dcda4381de086ab5ec''
		]
	},
	HiExampleCommit {
		#id : ''49b89cd6b529a27453cbb52d2962861f26b4ac01'',
		#comment : ''[Bug]: Default Source Code Subfolder Should Be \''source\'', Not \''repository\'' '',
		#parentIds : [
			''9ba14e9791c2d255ce24dbdc36a871b0f83037f6''
		]
	},
	HiExampleCommit {
		#id : ''9ba14e9791c2d255ce24dbdc36a871b0f83037f6'',
		#comment : ''[ENH]: Fix #471 - Setting for Default Code Subdirectory\n\n- Example: `IceRepository defaultCodeSubdirectory: aString "e.g. \''src\''"`\n- Default to \''source\'' per http://forum.world.st/Beacon-moved-to-github-tp4953881p4954017.html\n- Also available via Settings Browers'',
		#parentIds : [
			''fef1f906874542ea64c666404084d3f900559fe3''
		]
	},
	HiExampleCommit {
		#id : ''5a01697b959969704c6b40dcda4381de086ab5ec'',
		#comment : ''metacello baselines now can be executed also from repository context menu\nbaseline plugin renamed to metacello plugin'',
		#parentIds : [
			''74887bef3ad8286dc041e7703c095af4edf4a30f''
		]
	},
	HiExampleCommit {
		#id : ''74887bef3ad8286dc041e7703c095af4edf4a30f'',
		#comment : ''fix tests'',
		#parentIds : [
			''d84642cbc638d054fab6ea2ad3a4ab0b6e4a9584''
		]
	},
	HiExampleCommit {
		#id : ''d84642cbc638d054fab6ea2ad3a4ab0b6e4a9584'',
		#comment : ''Merge with 9559467353fe58a3ccbd33c99c364b6682c5da16'',
		#parentIds : [
			''8f6f7220a7554ef3ec0dc02f25b6b02811dd9a22'',
			''9559467353fe58a3ccbd33c99c364b6682c5da16''
		]
	},
	HiExampleCommit {
		#id : ''8f6f7220a7554ef3ec0dc02f25b6b02811dd9a22'',
		#comment : ''do not use a fast table to show general info because it confuses glamour browser (and creates a lot of errors)\n\nclose #470'',
		#parentIds : [
			''0e1cdde39898684e4e4bdf24652023228e2a1d7d''
		]
	},
	HiExampleCommit {
		#id : ''9559467353fe58a3ccbd33c99c364b6682c5da16'',
		#comment : ''Merge pull request #455 from jecisc/cf_moveIcebergToMostUsedToolsInWorldMenu\n\ncf_moveIcebergToMostUsedToolsInWorldMenu'',
		#parentIds : [
			''d333bf66050aa8a4eef8fd0e7dd5844e0975e112'',
			''93b6c6d5d8a5968d5b7f134381a71453cc6d7498''
		]
	},
	HiExampleCommit {
		#id : ''d333bf66050aa8a4eef8fd0e7dd5844e0975e112'',
		#comment : ''Merge pull request #449 from jecisc/cf_packageSorting\n\ncf_packageSorting'',
		#parentIds : [
			''f9c11b69766de9baa11657bfa96ae772f5556c39'',
			''8d381bb981432d15057a83e93aa0f17cc5bcb71d''
		]
	},
	HiExampleCommit {
		#id : ''0e1cdde39898684e4e4bdf24652023228e2a1d7d'',
		#comment : ''add possibility of browsing not-loaded packages\n\nclose #239'',
		#parentIds : [
			''9f5194d7e272d8d1c71cc953be2efd45f52de925''
		]
	},
	HiExampleCommit {
		#id : ''9f5194d7e272d8d1c71cc953be2efd45f52de925'',
		#comment : ''add a "full sync" option.'',
		#parentIds : [
			''d3385720815a7415ee952abd021029f0125e7f34''
		]
	},
	HiExampleCommit {
		#id : ''d3385720815a7415ee952abd021029f0125e7f34'',
		#comment : ''fix a problem on cherry-pick and renamed/removed classes'',
		#parentIds : [
			''54af075a2f45e0bc965d3586de7a4000bbe81cb2''
		]
	},
	HiExampleCommit {
		#id : ''54af075a2f45e0bc965d3586de7a4000bbe81cb2'',
		#comment : ''renamed'',
		#parentIds : [
			''6a172ae3f530c975ab5d98b0d13fe207c1b5610f''
		]
	},
	HiExampleCommit {
		#id : ''6a172ae3f530c975ab5d98b0d13fe207c1b5610f'',
		#comment : ''if no remote, calculate repository name from location'',
		#parentIds : [
			''192e20fdc8e248b0951d6036f2314af53a8a0407''
		]
	},
	HiExampleCommit {
		#id : ''192e20fdc8e248b0951d6036f2314af53a8a0407'',
		#comment : ''fix adaptor propagation of monticello repositories\nclose #256'',
		#parentIds : [
			''1261291c16a83b4cee6ff36df9e1b15d24504210''
		]
	},
	HiExampleCommit {
		#id : ''1261291c16a83b4cee6ff36df9e1b15d24504210'',
		#comment : ''fix settings for formatType'',
		#parentIds : [
			''9d13132c5845d74a69246fe5bc29404dc6009626''
		]
	},
	HiExampleCommit {
		#id : ''9d13132c5845d74a69246fe5bc29404dc6009626'',
		#comment : ''add missing class comments and refactored some classes (to their correct places and/or correct names)'',
		#parentIds : [
			''36c684af2a828a933a1a5990987277b47ef2acac''
		]
	},
	HiExampleCommit {
		#id : ''36c684af2a828a933a1a5990987277b47ef2acac'',
		#comment : ''package needs to be an MCPackage'',
		#parentIds : [
			''de882cd4c6525e18ef21f87c197856101724b8c0''
		]
	},
	HiExampleCommit {
		#id : ''de882cd4c6525e18ef21f87c197856101724b8c0'',
		#comment : ''add missing method for metacelllo compatibility'',
		#parentIds : [
			''8e1905bb9d21e66c674f47af78cdf26a6d6e45bf''
		]
	},
	HiExampleCommit {
		#id : ''8e1905bb9d21e66c674f47af78cdf26a6d6e45bf'',
		#comment : ''STON writer uses cr as new line by default, not platform line ending'',
		#parentIds : [
			''280fe446d41cebe9a331d4003cefe4df55379a79''
		]
	},
	HiExampleCommit {
		#id : ''280fe446d41cebe9a331d4003cefe4df55379a79'',
		#comment : '' fixed diff for tonel/filetree formats'',
		#parentIds : [
			''301cd1a5d4a00570c687bcf5fa8d28079d1fcc12''
		]
	},
	HiExampleCommit {
		#id : ''301cd1a5d4a00570c687bcf5fa8d28079d1fcc12'',
		#comment : ''removed'',
		#parentIds : [
			''b9cba0570623ef04b4f092e2330d3ce7b008259d''
		]
	},
	HiExampleCommit {
		#id : ''b9cba0570623ef04b4f092e2330d3ce7b008259d'',
		#comment : ''include package deletion (but this may cause problems, I need to back later)'',
		#parentIds : [
			''ecb1cd49ea71722d6d6513d3fa964e2772cf8c2a''
		]
	},
	HiExampleCommit {
		#id : ''ecb1cd49ea71722d6d6513d3fa964e2772cf8c2a'',
		#comment : ''remove old file utils'',
		#parentIds : [
			''033fe211e73adac934e5d145fa9b442e7760b041''
		]
	},
	HiExampleCommit {
		#id : ''033fe211e73adac934e5d145fa9b442e7760b041'',
		#comment : ''refactored file utils'',
		#parentIds : [
			''4e58383b31b5c398398bed453cbe50ec9ee2dc4e''
		]
	},
	HiExampleCommit {
		#id : ''f9c11b69766de9baa11657bfa96ae772f5556c39'',
		#comment : ''Merge pull request #460 from StephanEggermont/patch-1\n\ntypo'',
		#parentIds : [
			''1c2017e3e2dbaae229fea0754f58b9b9ca6e0f2e'',
			''d3bf34bf5a2c3b9bfd89d746f4ae6324b209ea45''
		]
	},
	HiExampleCommit {
		#id : ''1c2017e3e2dbaae229fea0754f58b9b9ca6e0f2e'',
		#comment : ''Merge pull request #462 from StephanEggermont/patch-3\n\ntypo'',
		#parentIds : [
			''d55dd7dac882e23be086c3630fa8b72a534d95ed'',
			''ff0ec272d4883e35f4d14e0bed73665ae3e8bece''
		]
	},
	HiExampleCommit {
		#id : ''ff0ec272d4883e35f4d14e0bed73665ae3e8bece'',
		#comment : ''typo'',
		#parentIds : [
			''fef1f906874542ea64c666404084d3f900559fe3''
		]
	},
	HiExampleCommit {
		#id : ''d3bf34bf5a2c3b9bfd89d746f4ae6324b209ea45'',
		#comment : ''typo'',
		#parentIds : [
			''fef1f906874542ea64c666404084d3f900559fe3''
		]
	},
	HiExampleCommit {
		#id : ''fef1f906874542ea64c666404084d3f900559fe3'',
		#comment : ''Merge pull request #459 from pharo-vcs/dev-0.5\n\nsmall fixes'',
		#parentIds : [
			''a827568dbd66b359b74d9c142fb90b0658a844ef'',
			''d55dd7dac882e23be086c3630fa8b72a534d95ed''
		]
	},
	HiExampleCommit {
		#id : ''4e58383b31b5c398398bed453cbe50ec9ee2dc4e'',
		#comment : ''generalise file format to use (including setting) '',
		#parentIds : [
			''4caa2e3d9e4f18703b5ae3d401d36a0abf3dc269''
		]
	},
	HiExampleCommit {
		#id : ''4caa2e3d9e4f18703b5ae3d401d36a0abf3dc269'',
		#comment : ''extracted libgit-filetree and libgit-tonel to separated packages\nadd setting for chosing file format for new repositories.\nupdate baseline.\n'',
		#parentIds : [
			''2f92ee779a70f0f4e99a6fec18cdf14c18d730c4''
		]
	},
	HiExampleCommit {
		#id : ''2f92ee779a70f0f4e99a6fec18cdf14c18d730c4'',
		#comment : ''using filtree package on baseline'',
		#parentIds : [
			''3efcc84862b973e067bed83257c9f0ed92bd7ccf''
		]
	},
	HiExampleCommit {
		#id : ''3efcc84862b973e067bed83257c9f0ed92bd7ccf'',
		#comment : ''classes renamed from cypress to filetree'',
		#parentIds : [
			''897543b79fc75cc57f2d19e4306114f2a949f512''
		]
	},
	HiExampleCommit {
		#id : ''897543b79fc75cc57f2d19e4306114f2a949f512'',
		#comment : ''removed cypress package to filetree (is more alike what we use) '',
		#parentIds : [
			''9fe92b761e95b822cb58f942cde9f4ebb3357fe0''
		]
	},
	HiExampleCommit {
		#id : ''9fe92b761e95b822cb58f942cde9f4ebb3357fe0'',
		#comment : ''directory name has to answer always a string'',
		#parentIds : [
			''44f0fb3fd7403594dcc1f10ba14d9c2879474734''
		]
	},
	HiExampleCommit {
		#id : ''44f0fb3fd7403594dcc1f10ba14d9c2879474734'',
		#comment : ''split cypress and tonel formats from libgit'',
		#parentIds : [
			''a3bf2d7bba1c7b5c8ce42c99506a33e564723963''
		]
	},
	HiExampleCommit {
		#id : ''a3bf2d7bba1c7b5c8ce42c99506a33e564723963'',
		#comment : ''fixed method'',
		#parentIds : [
			''141b8c4bf58a4fb5839b4d43fe8c6a2f056d9c79''
		]
	},
	HiExampleCommit {
		#id : ''141b8c4bf58a4fb5839b4d43fe8c6a2f056d9c79'',
		#comment : ''fix one test break another :)'',
		#parentIds : [
			''522c3d6c9c012898838835fb0d12a4b8f5bbbde9''
		]
	},
	HiExampleCommit {
		#id : ''d55dd7dac882e23be086c3630fa8b72a534d95ed'',
		#comment : ''Merge pull request #456 from jecisc/cf_useThemeForColors\n\nfix(ui): do not use hardcoded colors for Iceberg'',
		#parentIds : [
			''ae89d477b16592b42c453d825cd36a1bdb870203'',
			''263562f2b855212afd9bb1a89ea545dfce62dac8''
		]
	},
	HiExampleCommit {
		#id : ''ae89d477b16592b42c453d825cd36a1bdb870203'',
		#comment : ''Merge pull request #458 from astares/20401-IceSemanticVersionProtocols\n\n20401 Categorization - IceSemanticVersion should follow Object protocol'',
		#parentIds : [
			''f1172d42ae3e60ce2fb16ad13827d103fb2c44e2'',
			''fcfcd42d43db14e49df19f8347ace671496a14ef''
		]
	},
	HiExampleCommit {
		#id : ''522c3d6c9c012898838835fb0d12a4b8f5bbbde9'',
		#comment : ''fix existing tests'',
		#parentIds : [
			''ca71d459591a91abee4fb445a5fb64a73b02a2cb''
		]
	},
	HiExampleCommit {
		#id : ''ca71d459591a91abee4fb445a5fb64a73b02a2cb'',
		#comment : ''compact dictionary print: if there is just one element, do not use 3 lines but just one.'',
		#parentIds : [
			''df0e20d83f425c01adbe6b21dfeb2575c3e981fe''
		]
	},
	HiExampleCommit {
		#id : ''df0e20d83f425c01adbe6b21dfeb2575c3e981fe'',
		#comment : ''split needs to take care about `stream atEnd`'',
		#parentIds : [
			''1773a0463d42e1dce7605b8538e624c9b810f5c7''
		]
	},
	HiExampleCommit {
		#id : ''fcfcd42d43db14e49df19f8347ace671496a14ef'',
		#comment : ''Categorization - IceSemanticVersion should follow Object protocol\n'',
		#parentIds : [
			''f1172d42ae3e60ce2fb16ad13827d103fb2c44e2''
		]
	},
	HiExampleCommit {
		#id : ''1773a0463d42e1dce7605b8538e624c9b810f5c7'',
		#comment : ''split method seems to be working as expected'',
		#parentIds : [
			''2a3f7d5e09fb8d5bcc3a07cd31910d5d00f4b737''
		]
	},
	HiExampleCommit {
		#id : ''2a3f7d5e09fb8d5bcc3a07cd31910d5d00f4b737'',
		#comment : ''fixing split method (WIP) and small fixes'',
		#parentIds : [
			''26b272081232fb8ba9a29250eb92f49f4fc5ac70''
		]
	},
	HiExampleCommit {
		#id : ''26b272081232fb8ba9a29250eb92f49f4fc5ac70'',
		#comment : ''temporal fix for parse comment'',
		#parentIds : [
			''0f70df04fba5955df00dcf3a82b26498fcee916a''
		]
	},
	HiExampleCommit {
		#id : ''0f70df04fba5955df00dcf3a82b26498fcee916a'',
		#comment : ''work on fix parsing to ensure we have same sources.'',
		#parentIds : [
			''af6088fcee6f1c914e0aed13d6c2c0bed4e2fb85''
		]
	},
	HiExampleCommit {
		#id : ''af6088fcee6f1c914e0aed13d6c2c0bed4e2fb85'',
		#comment : ''extract cypress logic and generalize mechanisms (I feel this is not over)'',
		#parentIds : [
			''80443942b47f4e21e54d3f4a301c380726c86d86''
		]
	},
	HiExampleCommit {
		#id : ''80443942b47f4e21e54d3f4a301c380726c86d86'',
		#comment : ''first commit with tonel format working!'',
		#parentIds : [
			''3c3a1f497819f58769da8f084152910518367dde''
		]
	},
	HiExampleCommit {
		#id : ''3c3a1f497819f58769da8f084152910518367dde'',
		#comment : ''fixes to keep cypress working'',
		#parentIds : [
			''37512050d51e5a76a9772fc669cab497b8384076''
		]
	},
	HiExampleCommit {
		#id : ''37512050d51e5a76a9772fc669cab497b8384076'',
		#comment : ''moved to core category'',
		#parentIds : [
			''f2218095c7c757b97a97fee1fc23236c4469f9cd''
		]
	},
	HiExampleCommit {
		#id : ''f2218095c7c757b97a97fee1fc23236c4469f9cd'',
		#comment : ''fix name'',
		#parentIds : [
			''b86914b500ece3a18369f9d7882a57aaf5d90512''
		]
	},
	HiExampleCommit {
		#id : ''b86914b500ece3a18369f9d7882a57aaf5d90512'',
		#comment : ''changed ids for symbols'',
		#parentIds : [
			''f5ade85503365d809482954c31c2e44e5168a4f4''
		]
	},
	HiExampleCommit {
		#id : ''f5ade85503365d809482954c31c2e44e5168a4f4'',
		#comment : ''add first version of writer'',
		#parentIds : [
			''7f9613c4b75db8c871d7d0660d5c605a656c1be4''
		]
	},
	HiExampleCommit {
		#id : ''7f9613c4b75db8c871d7d0660d5c605a656c1be4'',
		#comment : ''changed name because it was ugly (still is, but a bit better)'',
		#parentIds : [
			''9e86bcb68626e304963d9e8e200e266a1431c78e''
		]
	},
	HiExampleCommit {
		#id : ''9e86bcb68626e304963d9e8e200e266a1431c78e'',
		#comment : ''refactor'',
		#parentIds : [
			''53486918f8955cc9839361accfccfe102d307f5c''
		]
	},
	HiExampleCommit {
		#id : ''53486918f8955cc9839361accfccfe102d307f5c'',
		#comment : ''refactor to accept different reader/writers'',
		#parentIds : [
			''57e9b0b4a1af0fd21e62d0d19d06382611eaf7bb''
		]
	},
	HiExampleCommit {
		#id : ''57e9b0b4a1af0fd21e62d0d19d06382611eaf7bb'',
		#comment : ''first version of TonelReader working'',
		#parentIds : [
			''475050e9b1b00d4da464fcd2d5506c8dc3b8ca28''
		]
	},
	HiExampleCommit {
		#id : ''475050e9b1b00d4da464fcd2d5506c8dc3b8ca28'',
		#comment : ''renamed to Tonel'',
		#parentIds : [
			''91f8900c92ca3629bf4a02e4927b1e50cad17b07''
		]
	},
	HiExampleCommit {
		#id : ''91f8900c92ca3629bf4a02e4927b1e50cad17b07'',
		#comment : ''refactor some names'',
		#parentIds : [
			''73574850569b04ef4cc274507cf3c26a25bf740e''
		]
	},
	HiExampleCommit {
		#id : ''73574850569b04ef4cc274507cf3c26a25bf740e'',
		#comment : ''moved to own category'',
		#parentIds : [
			''297efc797bc5b7ff319eb6f9626935051c26b8ec''
		]
	},
	HiExampleCommit {
		#id : ''297efc797bc5b7ff319eb6f9626935051c26b8ec'',
		#comment : ''add missing #type field for classes (not all classes are normal)'',
		#parentIds : [
			''90c47d755d278129c76e251d089d2705746c2a0e''
		]
	},
	HiExampleCommit {
		#id : ''90c47d755d278129c76e251d089d2705746c2a0e'',
		#comment : ''add ShouldIgnore for types (because of MCClassTraitDefinition) that does not seems to be needed'',
		#parentIds : [
			''f1d43a7a316ab900bfacf668a9ff3cfcf1b3811d''
		]
	},
	HiExampleCommit {
		#id : ''f1d43a7a316ab900bfacf668a9ff3cfcf1b3811d'',
		#comment : ''not used'',
		#parentIds : [
			''a006d04d0870cc1a3f0ed4eb1e7977d57564558c''
		]
	},
	HiExampleCommit {
		#id : ''a006d04d0870cc1a3f0ed4eb1e7977d57564558c'',
		#comment : ''not using this classes'',
		#parentIds : [
			''1673324bbb365c7aace705a8dd1e1c118d356a93''
		]
	},
	HiExampleCommit {
		#id : ''1673324bbb365c7aace705a8dd1e1c118d356a93'',
		#comment : ''version bump'',
		#parentIds : [
			''f1172d42ae3e60ce2fb16ad13827d103fb2c44e2''
		]
	},
	HiExampleCommit {
		#id : ''f1172d42ae3e60ce2fb16ad13827d103fb2c44e2'',
		#comment : ''add guessing of registered repositories to avoid unnecesary clones. '',
		#parentIds : [
			''1ec5df55ae600dfe12470fb790a64dfb2b33f006''
		]
	},
	HiExampleCommit {
		#id : ''1ec5df55ae600dfe12470fb790a64dfb2b33f006'',
		#comment : ''improve comparison between libgit versions'',
		#parentIds : [
			''5a1ae14408a42a57113cce4c0b2379a34b2b15dd''
		]
	},
	HiExampleCommit {
		#id : ''5a1ae14408a42a57113cce4c0b2379a34b2b15dd'',
		#comment : ''fix name'',
		#parentIds : [
			''d0956de24cab4503536b166e1cdab79e108e8abe''
		]
	},
	HiExampleCommit {
		#id : ''d0956de24cab4503536b166e1cdab79e108e8abe'',
		#comment : ''enhance commit message'',
		#parentIds : [
			''9606d7e390585e77d3dce41829f328f3c6b7f518''
		]
	},
	HiExampleCommit {
		#id : ''9606d7e390585e77d3dce41829f328f3c6b7f518'',
		#comment : ''introduce change again'',
		#parentIds : [
			''b65b6141e39bd38bde2de65b89b01e335016f20f''
		]
	},
	HiExampleCommit {
		#id : ''b65b6141e39bd38bde2de65b89b01e335016f20f'',
		#comment : ''add missing repository'',
		#parentIds : [
			''72dd45f5cb6e12754a8256757887b9a9995ea893''
		]
	},
	HiExampleCommit {
		#id : ''72dd45f5cb6e12754a8256757887b9a9995ea893'',
		#comment : ''trying to speed up commits by not traversing all index each time but getting diffs.'',
		#parentIds : [
			''c565efb693763be82660635ca838d8c224dbafb4''
		]
	},
	HiExampleCommit {
		#id : ''c565efb693763be82660635ca838d8c224dbafb4'',
		#comment : ''add "prepare for contribution" option to Pharo plugin.\n\nthe idea is to provide a common way to prepare the image and the repository before contributing. \n\nfor now, it only fetches pharo repository (to ensure we have all commits in hostory).'',
		#parentIds : [
			''87ede4c3c5174d1105dae2d1cf689b423466bfa6''
		]
	},
	HiExampleCommit {
		#id : ''87ede4c3c5174d1105dae2d1cf689b423466bfa6'',
		#comment : ''fixes to README.md\n'',
		#parentIds : [
			''0f5b0b71e880e228e55f9c87a2ace2b4ec5eb795''
		]
	},
	HiExampleCommit {
		#id : ''263562f2b855212afd9bb1a89ea545dfce62dac8'',
		#comment : ''Warning text color is not readable in White theme'',
		#parentIds : [
			''36ffa789dfae0119757ce9990e59dffe48c3cada''
		]
	},
	HiExampleCommit {
		#id : ''36ffa789dfae0119757ce9990e59dffe48c3cada'',
		#comment : ''fix(ui): do not use hardcoded colors for Iceberg\n\nSince the color used where added in Pharo 7, add a package to load them in Pharo 6 too.\n\nClose https://github.com/pharo-vcs/iceberg/issues/383'',
		#parentIds : [
			''0f5b0b71e880e228e55f9c87a2ace2b4ec5eb795''
		]
	},
	HiExampleCommit {
		#id : ''93b6c6d5d8a5968d5b7f134381a71453cc6d7498'',
		#comment : ''Since the order of most used tools is now fixed in Pharo 7, promote Iceberg to most used tools.'',
		#parentIds : [
			''c4eee2805559bdeecfdf84ca2fa845ff6f8acd9e''
		]
	},
	HiExampleCommit {
		#id : ''c4eee2805559bdeecfdf84ca2fa845ff6f8acd9e'',
		#comment : ''Merge branch \''dev-0.5\'' into cf_moveIcebergToMostUsedToolsInWorldMenu'',
		#parentIds : [
			''2a65506e13eac3b0909359103699ebcae9b6bcd6'',
			''0f5b0b71e880e228e55f9c87a2ace2b4ec5eb795''
		]
	},
	HiExampleCommit {
		#id : ''8d381bb981432d15057a83e93aa0f17cc5bcb71d'',
		#comment : ''Sorting of packages:\n\n- Sort the packages aphabetically\n- Add a sorting strategy. The user can customise the sorting to show the modified/not loaded packages first/last. \n- This customisation ban be done via the package tab of Iceberg or via settings\n\nFor now I set those defaults: \n- First: modified\n- Last: not loaded\n\nThis is my personnal choice. If it is too personal I can change it :)\nAdd missing method\nfoo\n\n'',
		#parentIds : [
			''e9de40ae36f87aac14048acff8a7ac3af03d857c''
		]
	},
	HiExampleCommit {
		#id : ''0f5b0b71e880e228e55f9c87a2ace2b4ec5eb795'',
		#comment : ''fix "refresh" in PR tool'',
		#parentIds : [
			''fdd4f4a6afb9c3efe654d80f86fd8ac0508759e2''
		]
	},
	HiExampleCommit {
		#id : ''a827568dbd66b359b74d9c142fb90b0658a844ef'',
		#comment : ''Merge pull request #454 from pharo-vcs/dev-0.5\n\nminor fixes'',
		#parentIds : [
			''0624ced27e70c4bff1552e53879ba2e16e419514'',
			''fdd4f4a6afb9c3efe654d80f86fd8ac0508759e2''
		]
	},
	HiExampleCommit {
		#id : ''fdd4f4a6afb9c3efe654d80f86fd8ac0508759e2'',
		#comment : ''Merge with 3088c7126206527079539f808ac133d308e3afd2'',
		#parentIds : [
			''c61ffc4b20cb745408340369cbbd4d36ab740b52'',
			''3088c7126206527079539f808ac133d308e3afd2''
		]
	},
	HiExampleCommit {
		#id : ''3088c7126206527079539f808ac133d308e3afd2'',
		#comment : ''Merge pull request #452 from theseion/450-cherry-picking-from-multiple-packages-mixes-up-methods\n\n450 cherry picking from multiple packages mixes up methods'',
		#parentIds : [
			''e9de40ae36f87aac14048acff8a7ac3af03d857c'',
			''d2550db3fd34ed54e8044cbf1617e94e894c92fe''
		]
	},
	HiExampleCommit {
		#id : ''c61ffc4b20cb745408340369cbbd4d36ab740b52'',
		#comment : ''add check against nil (because it may happens)'',
		#parentIds : [
			''1407d94aa8e332cd5937eba0b8ae3bdaac910077''
		]
	},
	HiExampleCommit {
		#id : ''d2550db3fd34ed54e8044cbf1617e94e894c92fe'',
		#comment : ''use package specific collection to create patch for cherry pick to prevent method mixups.'',
		#parentIds : [
			''54e4e55fd09cbd7a04b4d0d50b3f6a09f419b944''
		]
	},
	HiExampleCommit {
		#id : ''54e4e55fd09cbd7a04b4d0d50b3f6a09f419b944'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''e9de40ae36f87aac14048acff8a7ac3af03d857c'',
			''f12e31cc286dfb305c1cbc489947ccd6fce73631''
		]
	},
	HiExampleCommit {
		#id : ''e9de40ae36f87aac14048acff8a7ac3af03d857c'',
		#comment : ''Merge pull request #448 from estebanlm/add-default-comment-for-PR\n\nadd-default-comment-for-PR'',
		#parentIds : [
			''1407d94aa8e332cd5937eba0b8ae3bdaac910077'',
			''66bc7cb7f6ac73c7907d0633b0aa8c160fd46781''
		]
	},
	HiExampleCommit {
		#id : ''66bc7cb7f6ac73c7907d0633b0aa8c160fd46781'',
		#comment : ''getting last commit message as default test for PR'',
		#parentIds : [
			''1407d94aa8e332cd5937eba0b8ae3bdaac910077''
		]
	},
	HiExampleCommit {
		#id : ''0624ced27e70c4bff1552e53879ba2e16e419514'',
		#comment : ''Merge pull request #447 from pharo-vcs/dev-0.5\n\nfixes restore clone'',
		#parentIds : [
			''b7cc29702fd96db14b8673fb4fe977456441f7c3'',
			''1407d94aa8e332cd5937eba0b8ae3bdaac910077''
		]
	},
	HiExampleCommit {
		#id : ''1407d94aa8e332cd5937eba0b8ae3bdaac910077'',
		#comment : ''Merging with c03e07e7ab7ad50c4398e0bbb9c1874254857fe3'',
		#parentIds : [
			''4ab614b5a7d39093750083176166623cf8bf2b11'',
			''c03e07e7ab7ad50c4398e0bbb9c1874254857fe3''
		]
	},
	HiExampleCommit {
		#id : ''4ab614b5a7d39093750083176166623cf8bf2b11'',
		#comment : ''elements can be nil if GC happens'',
		#parentIds : [
			''a019ac9fbf0ee6b223e9e5acbc3f204f4822aac7''
		]
	},
	HiExampleCommit {
		#id : ''c03e07e7ab7ad50c4398e0bbb9c1874254857fe3'',
		#comment : ''even if there is an intromission from "IceLog" class that need to be reviewed\n\n'',
		#parentIds : [
			''473c38acae9173e4ca95406ef29a43913963b059'',
			''18dfa9cad5f61139f830e79206ba7e230ebc7636''
		]
	},
	HiExampleCommit {
		#id : ''18dfa9cad5f61139f830e79206ba7e230ebc7636'',
		#comment : ''do not cache the plugin manager (it does not worths, and can cause problems)'',
		#parentIds : [
			''d7068d0e9750eb7031d0af8b961440acc0ec3553''
		]
	},
	HiExampleCommit {
		#id : ''d7068d0e9750eb7031d0af8b961440acc0ec3553'',
		#comment : ''split clone from checkout (so I do it just once when a branch is explicit)'',
		#parentIds : [
			''e57942bf26799849075cef7e0a2ad93163317a6a''
		]
	},
	HiExampleCommit {
		#id : ''e57942bf26799849075cef7e0a2ad93163317a6a'',
		#comment : ''add restore repository dialog'',
		#parentIds : [
			''a019ac9fbf0ee6b223e9e5acbc3f204f4822aac7''
		]
	},
	HiExampleCommit {
		#id : ''473c38acae9173e4ca95406ef29a43913963b059'',
		#comment : ''Merge pull request #444 from jecisc/cf_RemoveSendersOfSubscribeSendTo\n\nChange senders of #subscribe:send:to: to use #when:send:to:'',
		#parentIds : [
			''e85ea8ea6616d876c80572d89af7a482b2d323f7'',
			''7be67c026f27dc3b37ac8e6ab967956831da5428''
		]
	},
	HiExampleCommit {
		#id : ''7be67c026f27dc3b37ac8e6ab967956831da5428'',
		#comment : ''Change senders of #subscribe:send:to: to use #when:send:to:'',
		#parentIds : [
			''e85ea8ea6616d876c80572d89af7a482b2d323f7''
		]
	},
	HiExampleCommit {
		#id : ''a019ac9fbf0ee6b223e9e5acbc3f204f4822aac7'',
		#comment : ''starting to work on a "log" functionality.'',
		#parentIds : [
			''5e1e3a24b18900d4d2a87a36c943ae4e97b41588''
		]
	},
	HiExampleCommit {
		#id : ''5e1e3a24b18900d4d2a87a36c943ae4e97b41588'',
		#comment : ''refactored to make things simpler'',
		#parentIds : [
			''0b68a3f4a3fdccb4592f544837198bb9aa7acba5''
		]
	},
	HiExampleCommit {
		#id : ''e85ea8ea6616d876c80572d89af7a482b2d323f7'',
		#comment : ''Merge pull request #422 from jecisc/cf_guessLocalRepositorySubDirectory\n\nGuess the subdirectory while importing a local repository.'',
		#parentIds : [
			''0b68a3f4a3fdccb4592f544837198bb9aa7acba5'',
			''901fb7f84134218f0d78a295dcd8ec7f25ee1176''
		]
	},
	HiExampleCommit {
		#id : ''901fb7f84134218f0d78a295dcd8ec7f25ee1176'',
		#comment : ''Guess the subdirectory while importing a local repository.\n\nSee: https://github.com/pharo-vcs/iceberg/issues/421\nTaking into account Esteban\''s comments'',
		#parentIds : [
			''1823ddee9804b6f3ba37135cda1a62b549acd972''
		]
	},
	HiExampleCommit {
		#id : ''0b68a3f4a3fdccb4592f544837198bb9aa7acba5'',
		#comment : ''using a WeakSet instead a WeakValueDictionary to keep models (seems to be better for clean up)'',
		#parentIds : [
			''1823ddee9804b6f3ba37135cda1a62b549acd972''
		]
	},
	HiExampleCommit {
		#id : ''b7cc29702fd96db14b8673fb4fe977456441f7c3'',
		#comment : ''Merge pull request #419 from pharo-vcs/dev-0.5\n\nfixes'',
		#parentIds : [
			''277847154913c59fbbeaa2061c35676932724bfe'',
			''1823ddee9804b6f3ba37135cda1a62b549acd972''
		]
	},
	HiExampleCommit {
		#id : ''1823ddee9804b6f3ba37135cda1a62b549acd972'',
		#comment : ''demeters law'',
		#parentIds : [
			''253bd3fe8cdaad483a6b64362755bb732ce3092a''
		]
	},
	HiExampleCommit {
		#id : ''253bd3fe8cdaad483a6b64362755bb732ce3092a'',
		#comment : ''changes vs. destination needs to take merge base to compare'',
		#parentIds : [
			''6bac5092c1def6a7f506dabca11d7736de606e4d''
		]
	},
	HiExampleCommit {
		#id : ''6bac5092c1def6a7f506dabca11d7736de606e4d'',
		#comment : ''create button now is disabled until an issue is entered'',
		#parentIds : [
			''f59e563fa389ad6a8df2624270725d87b6ca8458''
		]
	},
	HiExampleCommit {
		#id : ''f59e563fa389ad6a8df2624270725d87b6ca8458'',
		#comment : ''use #allTypes to get correct repostory type'',
		#parentIds : [
			''7c41f7be9359e8125eee20ab3ac7cfc68e605cae''
		]
	},
	HiExampleCommit {
		#id : ''7c41f7be9359e8125eee20ab3ac7cfc68e605cae'',
		#comment : ''add bitbucket and gitlab to provider-based repositories'',
		#parentIds : [
			''160cd9933f38a19f1df36e88add3b774cd93973c''
		]
	},
	HiExampleCommit {
		#id : ''160cd9933f38a19f1df36e88add3b774cd93973c'',
		#comment : ''add compatibleTimestamp method (not happy with the name) '',
		#parentIds : [
			''6abab22f8c53dd85d817ab3c3542489bf023d4e9''
		]
	},
	HiExampleCommit {
		#id : ''6abab22f8c53dd85d817ab3c3542489bf023d4e9'',
		#comment : ''added setLongpaths:global: '',
		#parentIds : [
			''f41262d5ba1a4bdbbc8bcf1a85992f819c08a39b''
		]
	},
	HiExampleCommit {
		#id : ''277847154913c59fbbeaa2061c35676932724bfe'',
		#comment : ''Merge pull request #418 from pharo-vcs/dev-0.5\n\nfix on IceRepository>>#location: '',
		#parentIds : [
			''d70b187282b3a3e688df0847390445485278daf5'',
			''f41262d5ba1a4bdbbc8bcf1a85992f819c08a39b''
		]
	},
	HiExampleCommit {
		#id : ''f41262d5ba1a4bdbbc8bcf1a85992f819c08a39b'',
		#comment : ''fix on location:, force to refresh name when changing locations (maybe an error? we\''ll see) '',
		#parentIds : [
			''b2189dc0b7c4fc73999305ffac2a8e72cdc87aa3''
		]
	},
	HiExampleCommit {
		#id : ''d70b187282b3a3e688df0847390445485278daf5'',
		#comment : ''Merge pull request #417 from pharo-vcs/dev-0.5\n\nimprove stability'',
		#parentIds : [
			''26e49b0485146c74eaaa406d849caa92b54acf62'',
			''b2189dc0b7c4fc73999305ffac2a8e72cdc87aa3''
		]
	},
	HiExampleCommit {
		#id : ''b2189dc0b7c4fc73999305ffac2a8e72cdc87aa3'',
		#comment : ''fix isAncestorOf:'',
		#parentIds : [
			''9d9d6d423933d0705192c9535be0cdc053af0d69''
		]
	},
	HiExampleCommit {
		#id : ''9d9d6d423933d0705192c9535be0cdc053af0d69'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''7a22f5e9b95354c0bfa5475c39c483e91b2cb17f'',
			''17d3d73024e60c65c40d8a7aa14f5ea6ef4e9781''
		]
	},
	HiExampleCommit {
		#id : ''7a22f5e9b95354c0bfa5475c39c483e91b2cb17f'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''c48ece4be590f3daf58a32f5a2f1cbab55031a13'',
			''17d3d73024e60c65c40d8a7aa14f5ea6ef4e9781''
		]
	},
	HiExampleCommit {
		#id : ''c48ece4be590f3daf58a32f5a2f1cbab55031a13'',
		#comment : ''Merge with 17d3d73024e60c65c40d8a7aa14f5ea6ef4e9781'',
		#parentIds : [
			''631062fbe30da38b2cda32dfbd903c8bee4c20c0'',
			''17d3d73024e60c65c40d8a7aa14f5ea6ef4e9781''
		]
	},
	HiExampleCommit {
		#id : ''631062fbe30da38b2cda32dfbd903c8bee4c20c0'',
		#comment : ''add a validation of required libgit2 to prevent opening iceberg browser (to prevent undesirable crashes) '',
		#parentIds : [
			''56aedbd79e5472cf7334059ea6a360218c2a9023''
		]
	},
	HiExampleCommit {
		#id : ''17d3d73024e60c65c40d8a7aa14f5ea6ef4e9781'',
		#comment : ''Merge pull request #399 from jecisc/cf_updateReadme\n\nUpdate Readme'',
		#parentIds : [
			''56aedbd79e5472cf7334059ea6a360218c2a9023'',
			''3628513d614f3ea6213d75a2c6133282e62696e3''
		]
	},
	HiExampleCommit {
		#id : ''26e49b0485146c74eaaa406d849caa92b54acf62'',
		#comment : ''Merge pull request #416 from pharo-vcs/dev-0.5\n\noptimise loading/merge/pull times'',
		#parentIds : [
			''4c51bd30f07e28806a67ba2ee88e5f4becdc6dc1'',
			''56aedbd79e5472cf7334059ea6a360218c2a9023''
		]
	},
	HiExampleCommit {
		#id : ''56aedbd79e5472cf7334059ea6a360218c2a9023'',
		#comment : ''add "system repository" concept, to be hidden by default.\nyou now can enable system repositories in settings, to turn them visible. \nPharo developers should have this by default.'',
		#parentIds : [
			''01ecc4b111a18195185b7eaa41b3012842e4e71e''
		]
	},
	HiExampleCommit {
		#id : ''01ecc4b111a18195185b7eaa41b3012842e4e71e'',
		#comment : ''added some validations to ensure no DNU is thrown when a repository is invalid. \nadded the posibility of clone the repository if missing.'',
		#parentIds : [
			''31f6bae8c8c7b472496d0a65235f7d81810c6006''
		]
	},
	HiExampleCommit {
		#id : ''31f6bae8c8c7b472496d0a65235f7d81810c6006'',
		#comment : ''Merge pull request #415 from noha/dev-0.5\n\nFixed multilevel submodule directory scanning'',
		#parentIds : [
			''44cf0b3318b0eeba6a9ca397c82c46e214ea85f6'',
			''173c3f623f3107d80d29433733caa88820398658''
		]
	},
	HiExampleCommit {
		#id : ''44cf0b3318b0eeba6a9ca397c82c46e214ea85f6'',
		#comment : ''fix bad name and typo'',
		#parentIds : [
			''9e6fdd2f91c5570ac810f9f08e02e845aeb18082''
		]
	},
	HiExampleCommit {
		#id : ''9e6fdd2f91c5570ac810f9f08e02e845aeb18082'',
		#comment : ''optimised load, merge and pull operations (taking just changed packages instead the full loaded history)'',
		#parentIds : [
			''f521b34d2bbe369184daed6f46a4b3c3f4632256''
		]
	},
	HiExampleCommit {
		#id : ''f521b34d2bbe369184daed6f46a4b3c3f4632256'',
		#comment : ''change implementation of #isAncestorOf: to not traverse the full tree of commmits each time.'',
		#parentIds : [
			''14c074b200ef31e221d7ab9a412a6e69fcfa2729''
		]
	},
	HiExampleCommit {
		#id : ''173c3f623f3107d80d29433733caa88820398658'',
		#comment : ''Fix multi-level submodule directory'',
		#parentIds : [
			''14c074b200ef31e221d7ab9a412a6e69fcfa2729''
		]
	},
	HiExampleCommit {
		#id : ''14c074b200ef31e221d7ab9a412a6e69fcfa2729'',
		#comment : ''add posibility of review pull requests from other remotes than "origin" '',
		#parentIds : [
			''f96928e5ac845887a4fcf087bd9e3457ad7bcd8a''
		]
	},
	HiExampleCommit {
		#id : ''4c51bd30f07e28806a67ba2ee88e5f4becdc6dc1'',
		#comment : ''Merge pull request #410 from pharo-vcs/dev-0.5\n\n- group branches by remote\r\n- add filter to package tab\r\n- move iceberg to most used tools\r\n- minor fixes'',
		#parentIds : [
			''549c038759a33cb6e912b08b658fab1e01bb98b4'',
			''f96928e5ac845887a4fcf087bd9e3457ad7bcd8a''
		]
	},
	HiExampleCommit {
		#id : ''f96928e5ac845887a4fcf087bd9e3457ad7bcd8a'',
		#comment : ''Merge branch \''master\'' into dev-0.5'',
		#parentIds : [
			''b330b89bf9903d44ae81197bf0e3c0a8c48e8443'',
			''549c038759a33cb6e912b08b658fab1e01bb98b4''
		]
	},
	HiExampleCommit {
		#id : ''b330b89bf9903d44ae81197bf0e3c0a8c48e8443'',
		#comment : ''method not needed anymore'',
		#parentIds : [
			''5745335981c47d07110818843dd97abe77f73906''
		]
	},
	HiExampleCommit {
		#id : ''5745335981c47d07110818843dd97abe77f73906'',
		#comment : ''remove unused (and conflicting) methods'',
		#parentIds : [
			''09e94495c0a6c9c47128a85f24205152e883118a''
		]
	},
	HiExampleCommit {
		#id : ''09e94495c0a6c9c47128a85f24205152e883118a'',
		#comment : ''Merge pull request #408 from jecisc/cf_addFilterToPackageView\n\nDefine #packageName instead of  `package name`'',
		#parentIds : [
			''c4c8276df412b015b4ae069fe076f627d7abee96'',
			''c6a889bf50f9feda7835fbcc1e2b4e325697c983''
		]
	},
	HiExampleCommit {
		#id : ''2a65506e13eac3b0909359103699ebcae9b6bcd6'',
		#comment : ''Merge branch \''dev-0.5\'' into cf_moveIcebergToMostUsedToolsInWorldMenu'',
		#parentIds : [
			''0d5412185d3f221b7e4ff35ea3f6e3fa5965adf0'',
			''c4c8276df412b015b4ae069fe076f627d7abee96''
		]
	},
	HiExampleCommit {
		#id : ''c6a889bf50f9feda7835fbcc1e2b4e325697c983'',
		#comment : ''Define #packageName instead of  `package name`'',
		#parentIds : [
			''51e5861b4311c32023e36915b9b452de81b2c3a7''
		]
	},
	HiExampleCommit {
		#id : ''c4c8276df412b015b4ae069fe076f627d7abee96'',
		#comment : ''Merge pull request #330 from pavel-krivanek/issue329\n\nissue329, merged into dev-0.5 '',
		#parentIds : [
			''625ed7fc8201367df3f0cd10444095b091c8f8fe'',
			''bd937b7565541a4fa4e801a817cf24b45dfa391e''
		]
	},
	HiExampleCommit {
		#id : ''625ed7fc8201367df3f0cd10444095b091c8f8fe'',
		#comment : ''Merge pull request #394 from jecisc/cf_addFilterToPackageView\n\ncf_addFilterToPackageView'',
		#parentIds : [
			''a77d0d332df29fc948fbfe194d9310ac54f943b8'',
			''51e5861b4311c32023e36915b9b452de81b2c3a7''
		]
	},
	HiExampleCommit {
		#id : ''a77d0d332df29fc948fbfe194d9310ac54f943b8'',
		#comment : ''Merge pull request #392 from jecisc/cf_groupBranchesByRemote\n\n'',
		#parentIds : [
			''605833210c44756cb60f5ab3677092a14e5aaf95'',
			''6bc5bfb81f84475add46af753b18f0aa759e9c69''
		]
	},
	HiExampleCommit {
		#id : ''549c038759a33cb6e912b08b658fab1e01bb98b4'',
		#comment : ''Merge pull request #397 from jecisc/cf_registerIcebergInShortcuts\n\nAdd shortcut to open Iceberg. (meta o + meta i).'',
		#parentIds : [
			''7b103e852893f2c5225fb675379ae87ab5db573d'',
			''883b0f7f904fe5456791d009fd34d74a91715142''
		]
	},
	HiExampleCommit {
		#id : ''605833210c44756cb60f5ab3677092a14e5aaf95'',
		#comment : ''Merge pull request #405 from jecisc/cf_ensureDiffViewHasSameMetadata\n\nUse #diffFromSource and #diffToSource to ensure the metadata are bothâ\u0080¦'',
		#parentIds : [
			''7106e9f89101aec4893803a7329894425b7a5446'',
			''d721d278c6a461252a29380a44b7e313e9d757a4''
		]
	},
	HiExampleCommit {
		#id : ''7106e9f89101aec4893803a7329894425b7a5446'',
		#comment : ''Merge pull request #406 from jecisc/cf_moveIcebergToMostUsedToolsInWorldMenu\n\nMove Iceberg to most used tools in the world menu since it is the defâ\u0080¦'',
		#parentIds : [
			''07d0491450c50011c4a6fded10fbb26d3338fd05'',
			''0d5412185d3f221b7e4ff35ea3f6e3fa5965adf0''
		]
	},
	HiExampleCommit {
		#id : ''07d0491450c50011c4a6fded10fbb26d3338fd05'',
		#comment : ''Merge pull request #407 from jecisc/cf_removeUnusedVariables\n\nRemove unused variables.'',
		#parentIds : [
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99'',
			''c0e3d6cddca95fdc4cf396b9b78a70e306f44d3b''
		]
	},
	HiExampleCommit {
		#id : ''c0e3d6cddca95fdc4cf396b9b78a70e306f44d3b'',
		#comment : ''Remove unused variables. \n\nSee: https://github.com/pharo-vcs/iceberg/issues/379'',
		#parentIds : [
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99''
		]
	},
	HiExampleCommit {
		#id : ''0d5412185d3f221b7e4ff35ea3f6e3fa5965adf0'',
		#comment : ''Move Iceberg to most used tools in the world menu since it is the default in Pharo 7.\n\nSee: https://github.com/pharo-vcs/iceberg/issues/401'',
		#parentIds : [
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99''
		]
	},
	HiExampleCommit {
		#id : ''d721d278c6a461252a29380a44b7e313e9d757a4'',
		#comment : ''Use #diffFromSource and #diffToSource to ensure the metadata are both the same in the diff view. '',
		#parentIds : [
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99''
		]
	},
	HiExampleCommit {
		#id : ''3628513d614f3ea6213d75a2c6133282e62696e3'',
		#comment : ''Update Readme\n\n- Remove comment from when the dev branch was way more advanced than master branch\r\n- Update the update script'',
		#parentIds : [
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99''
		]
	},
	HiExampleCommit {
		#id : ''883b0f7f904fe5456791d009fd34d74a91715142'',
		#comment : ''Add shortcut to open Iceberg. (meta o + meta i).\n\nIssue related: https://github.com/pharo-vcs/iceberg/issues/390'',
		#parentIds : [
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99''
		]
	},
	HiExampleCommit {
		#id : ''51e5861b4311c32023e36915b9b452de81b2c3a7'',
		#comment : ''This add a filter to the package tab. It will filter if the package name includes the search.\n\nIssue related: https://github.com/pharo-vcs/iceberg/issues/393\nDo not be case sensitive. '',
		#parentIds : [
			''2094e011ab9239ef1f055e9f8395007ccc53fd5d''
		]
	},
	HiExampleCommit {
		#id : ''6bc5bfb81f84475add46af753b18f0aa759e9c69'',
		#comment : ''Do not display remote name in the branch view since they are now grouped by remote. aa'',
		#parentIds : [
			''243430177cad4fa163eebfe99641d10589bfcbf6''
		]
	},
	HiExampleCommit {
		#id : ''243430177cad4fa163eebfe99641d10589bfcbf6'',
		#comment : ''It is #allExpanded and not #shouldExpandAll\n\n'',
		#parentIds : [
			''137af72ac23e91a3abeebe2d23ed7e76b87734b9''
		]
	},
	HiExampleCommit {
		#id : ''137af72ac23e91a3abeebe2d23ed7e76b87734b9'',
		#comment : ''Put the local group as bold.'',
		#parentIds : [
			''d9aa5493dc2c7877b37279a93a203ddf32373b37''
		]
	},
	HiExampleCommit {
		#id : ''d9aa5493dc2c7877b37279a93a203ddf32373b37'',
		#comment : ''This group the branches by remotes in the branches view.\n\nIssue related: https://github.com/pharo-vcs/iceberg/issues/349\n\nKnown bug: The elements of the tree should be openned but for now the FastTreeTable of Glamour cannot do it. This is a Glamour bug and not an Icebeng one. See: https://pharo.fogbugz.com/f/cases/20229/GLMFastTreeTablePresentation-does-not-expand-the-elements-if-we-ask-it'',
		#parentIds : [
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99''
		]
	},
	HiExampleCommit {
		#id : ''7b103e852893f2c5225fb675379ae87ab5db573d'',
		#comment : ''Merge pull request #389 from pharo-vcs/dev-0.5\n\nadd "changes vs. destination" tab on PR tool.'',
		#parentIds : [
			''cdcecc2784dd60bb02d98261d95be4460d3f5b6a'',
			''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99''
		]
	},
	HiExampleCommit {
		#id : ''4f7a40bdb4a0ca5d4e397b4965b3cd468e522d99'',
		#comment : ''add "changes vs. destination tab on PR tool. '',
		#parentIds : [
			''11787a4e11565371d9fcecced6fb21123a7e4dba''
		]
	},
	HiExampleCommit {
		#id : ''cdcecc2784dd60bb02d98261d95be4460d3f5b6a'',
		#comment : ''Merge pull request #388 from pharo-vcs/dev-0.5\n\nfix baseline for real'',
		#parentIds : [
			''96f68b56220f1529f3097e03a5a6198931a813e6'',
			''11787a4e11565371d9fcecced6fb21123a7e4dba''
		]
	},
	HiExampleCommit {
		#id : ''11787a4e11565371d9fcecced6fb21123a7e4dba'',
		#comment : ''oops. Now?'',
		#parentIds : [
			''d1c690a0f5e30eeba8df3f69bb80003e9d2c5f9b''
		]
	},
	HiExampleCommit {
		#id : ''d1c690a0f5e30eeba8df3f69bb80003e9d2c5f9b'',
		#comment : ''*really* fix baseline (I hope)'',
		#parentIds : [
			''610d5d0818c9bb0b7701eafff4e98153c214a4b4''
		]
	},
	HiExampleCommit {
		#id : ''96f68b56220f1529f3097e03a5a6198931a813e6'',
		#comment : ''Merge pull request #387 from pharo-vcs/dev-0.5\n\nfix baseline'',
		#parentIds : [
			''139b24301dd44dad80c3c13042d5b00f5e088c6d'',
			''610d5d0818c9bb0b7701eafff4e98153c214a4b4''
		]
	},
	HiExampleCommit {
		#id : ''610d5d0818c9bb0b7701eafff4e98153c214a4b4'',
		#comment : ''fix baseline'',
		#parentIds : [
			''65fa0b1be12f23e4a719453979fb6b04c2064c86''
		]
	},
	HiExampleCommit {
		#id : ''139b24301dd44dad80c3c13042d5b00f5e088c6d'',
		#comment : ''Merge pull request #386 from pharo-vcs/dev-0.5\n\nadd PR plugin'',
		#parentIds : [
			''a9709af8f4deca4974679679119bdcc9789cb070'',
			''65fa0b1be12f23e4a719453979fb6b04c2064c86''
		]
	},
	HiExampleCommit {
		#id : ''65fa0b1be12f23e4a719453979fb6b04c2064c86'',
		#comment : ''add a couple of options more to PR tool\nadd comments\nit is working :)'',
		#parentIds : [
			''d10f4bfce9d4f422e4e0ea297eb44e48d8b32d53''
		]
	},
	HiExampleCommit {
		#id : ''d10f4bfce9d4f422e4e0ea297eb44e48d8b32d53'',
		#comment : ''enhanced a bot more the PR browser (add data to detail box)'',
		#parentIds : [
			''c12b25a45e8715cd6ab3f75997a313756a60ff39''
		]
	},
	HiExampleCommit {
		#id : ''c12b25a45e8715cd6ab3f75997a313756a60ff39'',
		#comment : ''enhancing pr tool'',
		#parentIds : [
			''d824dec90c4d225918c154d8a6f5f5859a798f8a''
		]
	},
	HiExampleCommit {
		#id : ''d824dec90c4d225918c154d8a6f5f5859a798f8a'',
		#comment : ''first version of PR tool'',
		#parentIds : [
			''17c6907d542599ba494a0b2fb23b575041d482dc''
		]
	},
	HiExampleCommit {
		#id : ''a9709af8f4deca4974679679119bdcc9789cb070'',
		#comment : ''Merge pull request #382 from SergeStinckwich/master\n\nRestore README.md from 0.4 branch'',
		#parentIds : [
			''0dc4755f86fc4225287cbd7228bb1bc41ddb45e9'',
			''066bb3207aafa0b39f5548169c20416c3031743c''
		]
	},
	HiExampleCommit {
		#id : ''066bb3207aafa0b39f5548169c20416c3031743c'',
		#comment : ''Merge branch \''master\'' into master'',
		#parentIds : [
			''035fda65144099d9635696db489d1962e92a95b5'',
			''0dc4755f86fc4225287cbd7228bb1bc41ddb45e9''
		]
	},
	HiExampleCommit {
		#id : ''035fda65144099d9635696db489d1962e92a95b5'',
		#comment : ''Move FAQ before 5 min tutorial\n'',
		#parentIds : [
			''a03609232ce523f077ca2121714ef474cdfac41c''
		]
	},
	HiExampleCommit {
		#id : ''0dc4755f86fc4225287cbd7228bb1bc41ddb45e9'',
		#comment : ''Merge pull request #375 from macta/patch-1\n\nUpdate README.md'',
		#parentIds : [
			''17c6907d542599ba494a0b2fb23b575041d482dc'',
			''743808018a26362ad9b4a68a97c219de5362922c''
		]
	},
	HiExampleCommit {
		#id : ''a03609232ce523f077ca2121714ef474cdfac41c'',
		#comment : ''Restore README.md from 0.4 version\n'',
		#parentIds : [
			''17c6907d542599ba494a0b2fb23b575041d482dc''
		]
	},
	HiExampleCommit {
		#id : ''743808018a26362ad9b4a68a97c219de5362922c'',
		#comment : ''Merge branch \''master\'' into patch-1'',
		#parentIds : [
			''9b3367922d6040557c31aa7b2df7fdc2adbdc0e7'',
			''17c6907d542599ba494a0b2fb23b575041d482dc''
		]
	},
	HiExampleCommit {
		#id : ''17c6907d542599ba494a0b2fb23b575041d482dc'',
		#comment : ''fix wrongly flattened traits'',
		#parentIds : [
			''b5414453842d40241a02974c19cabe2c1cfa1446''
		]
	},
	HiExampleCommit {
		#id : ''b5414453842d40241a02974c19cabe2c1cfa1446'',
		#comment : ''Merging with f8fd0b589a921b10cc748b5e26a82ff13d10b8db'',
		#parentIds : [
			''8222f7c81718597466a4ec3aeea0ec3be6923506'',
			''f8fd0b589a921b10cc748b5e26a82ff13d10b8db''
		]
	},
	HiExampleCommit {
		#id : ''8222f7c81718597466a4ec3aeea0ec3be6923506'',
		#comment : ''add missing method'',
		#parentIds : [
			''2094e011ab9239ef1f055e9f8395007ccc53fd5d''
		]
	},
	HiExampleCommit {
		#id : ''2094e011ab9239ef1f055e9f8395007ccc53fd5d'',
		#comment : ''update baseline with correct libgit version'',
		#parentIds : [
			''4daefc9a49062ec5dcadb55f0717c5c17dcde3e6''
		]
	},
	HiExampleCommit {
		#id : ''4daefc9a49062ec5dcadb55f0717c5c17dcde3e6'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''d14213f1b7600262212c0954c0f4cc6f7eada185'',
			''f78f161c4eb21865f34f0f59d54f2a6f854a7289''
		]
	},
	HiExampleCommit {
		#id : ''d14213f1b7600262212c0954c0f4cc6f7eada185'',
		#comment : ''minor change for visibility'',
		#parentIds : [
			''61ef398d92c272d719bd0ed9b4c662eefaf99c60''
		]
	},
	HiExampleCommit {
		#id : ''61ef398d92c272d719bd0ed9b4c662eefaf99c60'',
		#comment : ''removed remaining method'',
		#parentIds : [
			''f3a7ac6d5a8e8452ee3ce489cd36b1d7fa1dd685''
		]
	},
	HiExampleCommit {
		#id : ''f3a7ac6d5a8e8452ee3ce489cd36b1d7fa1dd685'',
		#comment : ''add missing method on adaptor'',
		#parentIds : [
			''75b03fee1556500a7e31a34ca7b86a8bf272255b''
		]
	},
	HiExampleCommit {
		#id : ''75b03fee1556500a7e31a34ca7b86a8bf272255b'',
		#comment : ''remove *again* IceExamples (it was recovered somewhere)'',
		#parentIds : [
			''f78f161c4eb21865f34f0f59d54f2a6f854a7289''
		]
	},
	HiExampleCommit {
		#id : ''f78f161c4eb21865f34f0f59d54f2a6f854a7289'',
		#comment : ''add missing adaptor method '',
		#parentIds : [
			''d671b6e1b37f3b5329e8cd0845308f17911937db''
		]
	},
	HiExampleCommit {
		#id : ''9b3367922d6040557c31aa7b2df7fdc2adbdc0e7'',
		#comment : ''Update README.md\n\nThe api for loading a package seems to have changed'',
		#parentIds : [
			''f8fd0b589a921b10cc748b5e26a82ff13d10b8db''
		]
	},
	HiExampleCommit {
		#id : ''d671b6e1b37f3b5329e8cd0845308f17911937db'',
		#comment : ''add validation of remote existing (needed when creating a new repository) '',
		#parentIds : [
			''eccc60512b329c6c56a563d2b42b58042de47fbf''
		]
	},
	HiExampleCommit {
		#id : ''eccc60512b329c6c56a563d2b42b58042de47fbf'',
		#comment : ''implement #packageNames for unborn banches (maybe need a revisit)'',
		#parentIds : [
			''c2c3bab1e529f230bf6cf36afb42ab55b4fe5540''
		]
	},
	HiExampleCommit {
		#id : ''c2c3bab1e529f230bf6cf36afb42ab55b4fe5540'',
		#comment : ''add a todo to remember to check all pick when I have time (which will be likely never... but well, a man can hope)'',
		#parentIds : [
			''8760d5d9b75f0bdf39ed27f1207ce4050affe89a''
		]
	},
	HiExampleCommit {
		#id : ''8760d5d9b75f0bdf39ed27f1207ce4050affe89a'',
		#comment : ''picking all correct operations to perform the cherry-pick patch'',
		#parentIds : [
			''2c429805ddda7dd8436dab752790c9d306914ae2''
		]
	},
	HiExampleCommit {
		#id : ''2c429805ddda7dd8436dab752790c9d306914ae2'',
		#comment : ''Commit dialog enhanced (still work to do there, heh)'',
		#parentIds : [
			''46b105edb37e1e384f347a45def83a06722d9dfd''
		]
	},
	HiExampleCommit {
		#id : ''46b105edb37e1e384f347a45def83a06722d9dfd'',
		#comment : ''now, remove some unnecesary messages'',
		#parentIds : [
			''bed1794048d8cb88ac9eee2a162bfe9245598fa5''
		]
	},
	HiExampleCommit {
		#id : ''bed1794048d8cb88ac9eee2a162bfe9245598fa5'',
		#comment : ''fix messages for commit/push'',
		#parentIds : [
			''8bec1b3b1f36e36a0a63faac69ad51cb10d03356''
		]
	},
	HiExampleCommit {
		#id : ''8bec1b3b1f36e36a0a63faac69ad51cb10d03356'',
		#comment : ''some cleanups'',
		#parentIds : [
			''fff70243318911b3ebe28c18c68091296c128286''
		]
	},
	HiExampleCommit {
		#id : ''fff70243318911b3ebe28c18c68091296c128286'',
		#comment : ''First version of cherry-pick working (still heavy WIP).\n'',
		#parentIds : [
			''da6e45487156c942b62b2128dbcbb86b8eb28bfc''
		]
	},
	HiExampleCommit {
		#id : ''da6e45487156c942b62b2128dbcbb86b8eb28bfc'',
		#comment : ''in first commit, we cannot do "changes against parent"'',
		#parentIds : [
			''19613bc2256d3f7e4912477d65b8cbec9f9e69e2''
		]
	},
	HiExampleCommit {
		#id : ''19613bc2256d3f7e4912477d65b8cbec9f9e69e2'',
		#comment : ''stagedPackages needs to verify for package loaded before (because in case we removed a file, it will not work as espected)'',
		#parentIds : [
			''a7898e56290518408c208a84825fdf16d2512a2c''
		]
	},
	HiExampleCommit {
		#id : ''a7898e56290518408c208a84825fdf16d2512a2c'',
		#comment : ''Merging with eaed4b4179920f3a2bb83eb21092ce14075dfd26'',
		#parentIds : [
			''8ac738eaa443df319cf5eb5eef13d045a1816878'',
			''eaed4b4179920f3a2bb83eb21092ce14075dfd26''
		]
	},
	HiExampleCommit {
		#id : ''8ac738eaa443df319cf5eb5eef13d045a1816878'',
		#comment : ''diff between loaded and commits (the usual "sync" case) now understand added packages (removals is still missing)'',
		#parentIds : [
			''5308472a1827728fdc097dde0b566248b6be035f''
		]
	},
	HiExampleCommit {
		#id : ''5308472a1827728fdc097dde0b566248b6be035f'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''7705c0d7700d6a23ba9059bd6cdaa6e6a78e3123'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''7705c0d7700d6a23ba9059bd6cdaa6e6a78e3123'',
		#comment : ''fixed collection of packages (now we are "almost"  sure is correctly collected).'',
		#parentIds : [
			''aab16984578c51e21bbec91793dffb4ac4fecc10''
		]
	},
	HiExampleCommit {
		#id : ''aab16984578c51e21bbec91793dffb4ac4fecc10'',
		#comment : ''using LGitDiff when possible is working. \nsome verifications and clean up still needed  '',
		#parentIds : [
			''e45cb1afac94f8012ef654670c5eadb36d450c19''
		]
	},
	HiExampleCommit {
		#id : ''e45cb1afac94f8012ef654670c5eadb36d450c19'',
		#comment : ''branch info now is taken from external repository (so they match). Otherwise is too confusing.'',
		#parentIds : [
			''43ba77f1021853344f195510fd74b6b971b53df1''
		]
	},
	HiExampleCommit {
		#id : ''43ba77f1021853344f195510fd74b6b971b53df1'',
		#comment : ''remove #currentBranch method (it was pointing to #branch anyway)'',
		#parentIds : [
			''f992e8276fcc9cbc1403a07000f63f8664647d03''
		]
	},
	HiExampleCommit {
		#id : ''eaed4b4179920f3a2bb83eb21092ce14075dfd26'',
		#comment : ''Merge pull request #366 from jecisc/cf_issue352\n\nContract the name of a branch to 20 characters for buttons'',
		#parentIds : [
			''3239db8972ad9c1a4b5018fdfecfabb68e373fd0'',
			''bcc2e317d0bef7aaa3979006a48c2ba6b9d4aac4''
		]
	},
	HiExampleCommit {
		#id : ''f992e8276fcc9cbc1403a07000f63f8664647d03'',
		#comment : ''Merging with 3239db8972ad9c1a4b5018fdfecfabb68e373fd0'',
		#parentIds : [
			''9cfd922eaebb23fea37dba286d4bd77913dfe3fa'',
			''3239db8972ad9c1a4b5018fdfecfabb68e373fd0''
		]
	},
	HiExampleCommit {
		#id : ''9cfd922eaebb23fea37dba286d4bd77913dfe3fa'',
		#comment : ''minor fixes'',
		#parentIds : [
			''bd627ba1b9ccee8d543a923c4f81956f9c040a88''
		]
	},
	HiExampleCommit {
		#id : ''bcc2e317d0bef7aaa3979006a48c2ba6b9d4aac4'',
		#comment : ''Contract the name of a branch to 20characters for buttons\nRevert export noise :(\n\n\nCorrect typo :(\n\n'',
		#parentIds : [
			''3239db8972ad9c1a4b5018fdfecfabb68e373fd0''
		]
	},
	HiExampleCommit {
		#id : ''f8fd0b589a921b10cc748b5e26a82ff13d10b8db'',
		#comment : ''fix install text (it was old)'',
		#parentIds : [
			''a2486842a0ae5734c10873f7513dca688a9bbab1''
		]
	},
	HiExampleCommit {
		#id : ''3239db8972ad9c1a4b5018fdfecfabb68e373fd0'',
		#comment : ''refactored out the guess of kind (now is method class of IceGitUsernameOrEmailNotFound)\n\nfixes #361'',
		#parentIds : [
			''f6bf2d9107c24d06be2efdd61a00ad3f511acd64''
		]
	},
	HiExampleCommit {
		#id : ''f6bf2d9107c24d06be2efdd61a00ad3f511acd64'',
		#comment : ''add message "Failed to parse signature - Signature cannot have an empty name or email" to guessing of IceGitUsernameOrEmailNotFound'',
		#parentIds : [
			''a5ac806801b2e3fc8b73fc6cc9e02c10d849512d''
		]
	},
	HiExampleCommit {
		#id : ''a5ac806801b2e3fc8b73fc6cc9e02c10d849512d'',
		#comment : ''Merge pull request #364 from Hockenberry/dev-0.4\n\nRebased #363 to dev-0.4'',
		#parentIds : [
			''277a90e1452b23ecad4e96233e67ab33559b3ce2'',
			''3ba3a4b050a6b7d027262b8dbd00d55a71fd292b''
		]
	},
	HiExampleCommit {
		#id : ''3ba3a4b050a6b7d027262b8dbd00d55a71fd292b'',
		#comment : ''URL parsing fix #363 \nminor fix to http URL parsing that allows \''.\'' to be part of the last path segment\n(from @theseion, rebased to dev-0.4 )'',
		#parentIds : [
			''277a90e1452b23ecad4e96233e67ab33559b3ce2''
		]
	},
	HiExampleCommit {
		#id : ''a2486842a0ae5734c10873f7513dca688a9bbab1'',
		#comment : ''typo'',
		#parentIds : [
			''ce527b14d787c80850578b29f8a61084c815eeb6''
		]
	},
	HiExampleCommit {
		#id : ''ce527b14d787c80850578b29f8a61084c815eeb6'',
		#comment : ''enhanced answer in FAQ'',
		#parentIds : [
			''7d6273b3c3fb80c8b9d2b2d54308ba029b9f1158''
		]
	},
	HiExampleCommit {
		#id : ''277a90e1452b23ecad4e96233e67ab33559b3ce2'',
		#comment : ''Merge pull request #358 from pavel-krivanek/pharoUpstreamDetection\n\npharoUpstreamDetection'',
		#parentIds : [
			''87094e23aa3f874e5e35f0f66b65980b025f12e0'',
			''0668a2746c2cf8a4046993be7ca9eb28acdb6fe9''
		]
	},
	HiExampleCommit {
		#id : ''7d6273b3c3fb80c8b9d2b2d54308ba029b9f1158'',
		#comment : ''Merge pull request #360 from macta/patch-1\n\nUpdate README.md'',
		#parentIds : [
			''3675bc4ff45a10590bf69f29c8de64dc842a6363'',
			''f4336bd4d656c694f8c47ec9eb09b7e517ae0c04''
		]
	},
	HiExampleCommit {
		#id : ''f4336bd4d656c694f8c47ec9eb09b7e517ae0c04'',
		#comment : ''Update README.md\n\nSome minor grammatical tweaks I spotted when reading this'',
		#parentIds : [
			''3675bc4ff45a10590bf69f29c8de64dc842a6363''
		]
	},
	HiExampleCommit {
		#id : ''0668a2746c2cf8a4046993be7ca9eb28acdb6fe9'',
		#comment : ''add comment about bad practice'',
		#parentIds : [
			''9c79d40341486c1df6205a721b45ccfa6d9d7b4a''
		]
	},
	HiExampleCommit {
		#id : ''87094e23aa3f874e5e35f0f66b65980b025f12e0'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''2bbe8900a551594a9acd66bb9fcf67566a0b0354'',
			''25bb448a2a30a2ebe64e11e1138ec00c74adde03''
		]
	},
	HiExampleCommit {
		#id : ''9c79d40341486c1df6205a721b45ccfa6d9d7b4a'',
		#comment : ''enable PharoFogbugzPlugin items only for repositories with pharo-project/pharo  in the origin or in remotes'',
		#parentIds : [
			''25bb448a2a30a2ebe64e11e1138ec00c74adde03''
		]
	},
	HiExampleCommit {
		#id : ''2bbe8900a551594a9acd66bb9fcf67566a0b0354'',
		#comment : ''Merge branch \''master\'' into dev-0.4\n\n* master:\n  Class instance variable start lowercase (IcebergAnnouncer -> icebergAnnouncer in class Iceberg)\n  add Q/A for ssh-agent\n\n# Conflicts:\n#\tIceberg.package/Iceberg.class/properties.json\n'',
		#parentIds : [
			''25bb448a2a30a2ebe64e11e1138ec00c74adde03'',
			''3675bc4ff45a10590bf69f29c8de64dc842a6363''
		]
	},
	HiExampleCommit {
		#id : ''3675bc4ff45a10590bf69f29c8de64dc842a6363'',
		#comment : ''Merge pull request #357 from astares/dev-0.4\n\nClass instance variable IcebergAnnouncer in Iceberg should start lowercase '',
		#parentIds : [
			''c8a1fa9b07eb356b0bbb14289809f9e6755fd48f'',
			''7cffda07d450bd814735d6b19278c678d011855e''
		]
	},
	HiExampleCommit {
		#id : ''25bb448a2a30a2ebe64e11e1138ec00c74adde03'',
		#comment : ''Merge pull request #356 from Hockenberry/patch-3\n\nCorrect IceNetworkRemote>>userName'',
		#parentIds : [
			''5281efc42c14b3bfc7d092fcdf697dba1d474e34'',
			''98a543603dcb014230e563e7590a671b795a0ec6''
		]
	},
	HiExampleCommit {
		#id : ''7cffda07d450bd814735d6b19278c678d011855e'',
		#comment : ''Class instance variable start lowercase (IcebergAnnouncer -> icebergAnnouncer in class Iceberg)\n'',
		#parentIds : [
			''e64d19f5a624dbcdc7626c89bd10b64e86c3b3b3''
		]
	},
	HiExampleCommit {
		#id : ''98a543603dcb014230e563e7590a671b795a0ec6'',
		#comment : ''Incorporate suggestion from @jecisc'',
		#parentIds : [
			''79393e6f71f0a87f86efaa6f7c6fbb6a6756e33f''
		]
	},
	HiExampleCommit {
		#id : ''79393e6f71f0a87f86efaa6f7c6fbb6a6756e33f'',
		#comment : ''Update userName.st\n\nThe method\r\nIceNetworkRemote>>userName\r\n^user ifNil: [ String empty ] ifNotNil: \''\''.\r\n\r\nshould be\r\n\r\nIceNetworkRemote>>userName\r\n^user ifNil: [ String empty ] ifNotNil: [ user ].'',
		#parentIds : [
			''5281efc42c14b3bfc7d092fcdf697dba1d474e34''
		]
	},
	HiExampleCommit {
		#id : ''c8a1fa9b07eb356b0bbb14289809f9e6755fd48f'',
		#comment : ''add Q/A for ssh-agent'',
		#parentIds : [
			''f9c63ee8b987fc0aa3de29b191799173a41f0233''
		]
	},
	HiExampleCommit {
		#id : ''f12e31cc286dfb305c1cbc489947ccd6fce73631'',
		#comment : ''minor fix to http URL parsing that allows \''.\'' to be part of the last path segment\n'',
		#parentIds : [
			''064371e65ec2384a0cb94c3beef581af44e10b65''
		]
	},
	HiExampleCommit {
		#id : ''5281efc42c14b3bfc7d092fcdf697dba1d474e34'',
		#comment : ''fix RPackage>>#iceRepository.\n\ncloses #353'',
		#parentIds : [
			''f9c63ee8b987fc0aa3de29b191799173a41f0233''
		]
	},
	HiExampleCommit {
		#id : ''bd627ba1b9ccee8d543a923c4f81956f9c040a88'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''81e3c1b08cef6e1ff596d108743c9c5001d9f50c'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''81e3c1b08cef6e1ff596d108743c9c5001d9f50c'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''75d933fe8d2c9e1f29ec2e246d6c34f663434d63'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''75d933fe8d2c9e1f29ec2e246d6c34f663434d63'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''f987b3b05799edb24243d815204c6a1fecc5d0b5'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''f987b3b05799edb24243d815204c6a1fecc5d0b5'',
		#comment : ''Merge with f9c63ee8b987fc0aa3de29b191799173a41f0233'',
		#parentIds : [
			''9df959c84bb50baa409d3dac284be7983f297187'',
			''f9c63ee8b987fc0aa3de29b191799173a41f0233''
		]
	},
	HiExampleCommit {
		#id : ''9df959c84bb50baa409d3dac284be7983f297187'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''143f88984ea13e98927992cdfe8e58cdede3cd40'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''143f88984ea13e98927992cdfe8e58cdede3cd40'',
		#comment : ''Merge branch \''master\'' into dev-0.5\n\n* master:\n  Add one extra test.\n  Remove pending halt.\n  Reference commit should be the latest of candidates instead of the oldes.\n  Prueba 4.\n  Prueba 3.\n  Pruebas sobre la UI.\n  Add another test.\n  Add failing test.\n\n# Conflicts:\n#\tIceberg-Tests.package/IceTestCredentialProvider.class/properties.json\n#\tIceberg-UI.package/GLMMorphicSwappedColorDiffRenderer.class/properties.json\n'',
		#parentIds : [
			''cd0781f92294c944767a9f60c47713808ef96e48'',
			''f9c63ee8b987fc0aa3de29b191799173a41f0233''
		]
	},
	HiExampleCommit {
		#id : ''f9c63ee8b987fc0aa3de29b191799173a41f0233'',
		#comment : ''Merge pull request #351 from pharo-vcs/fix-commit-does-not-register-latest-versions\n\nFix commit does not register latest versions'',
		#parentIds : [
			''ec0389eec1c62fdefa64516b9196bd17dd9042a3'',
			''e85f1c96f66fcfd8ea0ac1fb0515be6dbad218df''
		]
	},
	HiExampleCommit {
		#id : ''e85f1c96f66fcfd8ea0ac1fb0515be6dbad218df'',
		#comment : ''Add one extra test.'',
		#parentIds : [
			''54fbd3eb1e485869eea451037dcd5a998a017d8c''
		]
	},
	HiExampleCommit {
		#id : ''54fbd3eb1e485869eea451037dcd5a998a017d8c'',
		#comment : ''Remove pending halt.'',
		#parentIds : [
			''0c9ee65e9aba905911e567e1dc383fe75e7f2367''
		]
	},
	HiExampleCommit {
		#id : ''0c9ee65e9aba905911e567e1dc383fe75e7f2367'',
		#comment : ''Reference commit should be the latest of candidates instead of the oldes.'',
		#parentIds : [
			''5a705957bbe6756b91ba6d720375c9e0de487b88''
		]
	},
	HiExampleCommit {
		#id : ''5a705957bbe6756b91ba6d720375c9e0de487b88'',
		#comment : ''Prueba 4.'',
		#parentIds : [
			''1f4f1c81173c7748052524e872aeff21d7694dec''
		]
	},
	HiExampleCommit {
		#id : ''1f4f1c81173c7748052524e872aeff21d7694dec'',
		#comment : ''Prueba 3.'',
		#parentIds : [
			''ea7f3c7602b82bf284ddd5c2c5f21d2bca0bc37b''
		]
	},
	HiExampleCommit {
		#id : ''ea7f3c7602b82bf284ddd5c2c5f21d2bca0bc37b'',
		#comment : ''Pruebas sobre la UI.'',
		#parentIds : [
			''a5e80e8f9f2d071db2978e0f28f1cace88d424dd''
		]
	},
	HiExampleCommit {
		#id : ''cd0781f92294c944767a9f60c47713808ef96e48'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''1c5856242dfabf16d56ad60794b682b93ec01d38'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''1c5856242dfabf16d56ad60794b682b93ec01d38'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''62e2a0363f22be2af277cd9c1f265f95a470289c'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''62e2a0363f22be2af277cd9c1f265f95a470289c'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''d3390912b9726d88fbd0ddf40188fc242f763aae'',
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''d3390912b9726d88fbd0ddf40188fc242f763aae'',
		#comment : ''add fetch action to repository selection actions'',
		#parentIds : [
			''f6a831d730127cc977253f7a4cefbe13a4f03f6b''
		]
	},
	HiExampleCommit {
		#id : ''f6a831d730127cc977253f7a4cefbe13a4f03f6b'',
		#comment : ''Merging with ec0389eec1c62fdefa64516b9196bd17dd9042a3'',
		#parentIds : [
			''064371e65ec2384a0cb94c3beef581af44e10b65'',
			''ec0389eec1c62fdefa64516b9196bd17dd9042a3''
		]
	},
	HiExampleCommit {
		#id : ''a5e80e8f9f2d071db2978e0f28f1cace88d424dd'',
		#comment : ''Add another test.'',
		#parentIds : [
			''b971ba8365ed092678c8f2cc71c662e1cbd0e11d''
		]
	},
	HiExampleCommit {
		#id : ''b971ba8365ed092678c8f2cc71c662e1cbd0e11d'',
		#comment : ''Add failing test.'',
		#parentIds : [
			''ec0389eec1c62fdefa64516b9196bd17dd9042a3''
		]
	},
	HiExampleCommit {
		#id : ''ec0389eec1c62fdefa64516b9196bd17dd9042a3'',
		#comment : ''remove a bunch of methods that are located on traits'',
		#parentIds : [
			''64dd53ea288990466e2b94095d33fa25764c7100''
		]
	},
	HiExampleCommit {
		#id : ''64dd53ea288990466e2b94095d33fa25764c7100'',
		#comment : ''remove not used method'',
		#parentIds : [
			''ee529677a99ddf21ba373214c7357d9ee4c3ac33''
		]
	},
	HiExampleCommit {
		#id : ''ee529677a99ddf21ba373214c7357d9ee4c3ac33'',
		#comment : ''updated README with a new FAQ\n'',
		#parentIds : [
			''883763aa4cef0aaeaad27a3fe55d5f941a0682ca''
		]
	},
	HiExampleCommit {
		#id : ''883763aa4cef0aaeaad27a3fe55d5f941a0682ca'',
		#comment : ''remove IceExamples because they are not working anyway.'',
		#parentIds : [
			''9880fc8d24c2b9a48a04f2c73bbaf7a5c357d4dd''
		]
	},
	HiExampleCommit {
		#id : ''9880fc8d24c2b9a48a04f2c73bbaf7a5c357d4dd'',
		#comment : ''update README,md\n'',
		#parentIds : [
			''fefadb29eb5b613eeefb899c7e2fe559d4a7a82d''
		]
	},
	HiExampleCommit {
		#id : ''fefadb29eb5b613eeefb899c7e2fe559d4a7a82d'',
		#comment : ''Merge with 7969c6ce3a7f710c0dde704b6b7eb381a5f971fe'',
		#parentIds : [
			''3f3e063bf48ea42ec5366dc0782d8ae10dbf6bcd'',
			''7969c6ce3a7f710c0dde704b6b7eb381a5f971fe''
		]
	},
	HiExampleCommit {
		#id : ''3f3e063bf48ea42ec5366dc0782d8ae10dbf6bcd'',
		#comment : ''Merge with 5382135a1ba5ca0bf64acd56055eec2d1b02b5b7'',
		#parentIds : [
			''df86f59ebf097bad5751de4b856fdfa4e2ce2de6'',
			''5382135a1ba5ca0bf64acd56055eec2d1b02b5b7''
		]
	},
	HiExampleCommit {
		#id : ''7969c6ce3a7f710c0dde704b6b7eb381a5f971fe'',
		#comment : ''Merge pull request #346 from theseion/master\n\nadded optional slash prefix to URL pattern in IceScpRemote>>parseUrl'',
		#parentIds : [
			''5382135a1ba5ca0bf64acd56055eec2d1b02b5b7'',
			''535238f86f7b785bb0e07b9d0782989a47f3f790''
		]
	},
	HiExampleCommit {
		#id : ''df86f59ebf097bad5751de4b856fdfa4e2ce2de6'',
		#comment : ''Merge pull request #348 from jecisc/dev-0.4\n\nReplace "Push" checkbox by "Commit and Push" button'',
		#parentIds : [
			''55df702b6823def0d4b56caab572b6e5292b2cdd'',
			''8522afb16be21ad50de62ca08e7c85d7a543b132''
		]
	},
	HiExampleCommit {
		#id : ''8522afb16be21ad50de62ca08e7c85d7a543b132'',
		#comment : ''Merge remote-tracking branch \''pharo-vcs/dev-0.4\'' into dev-0.4'',
		#parentIds : [
			''408515be05994f3b08ed50a4ef76a60dec2d718f'',
			''55df702b6823def0d4b56caab572b6e5292b2cdd''
		]
	},
	HiExampleCommit {
		#id : ''408515be05994f3b08ed50a4ef76a60dec2d718f'',
		#comment : ''- Remove the setting to remember if the commits should be pushed\n- Remove the checkbox to know if a commit should be pushed\n- Add a new button "Commit and Push"\n- Remove methods duplicated from a Trait'',
		#parentIds : [
			''4d262680f3b602a16110a88251381d941f181e1e''
		]
	},
	HiExampleCommit {
		#id : ''55df702b6823def0d4b56caab572b6e5292b2cdd'',
		#comment : ''pharo plugin is available if cloned from official repo'',
		#parentIds : [
			''d4a856844fa4dd625cda4557b23a2253eab37a10''
		]
	},
	HiExampleCommit {
		#id : ''d4a856844fa4dd625cda4557b23a2253eab37a10'',
		#comment : ''filetree was also transferred to pharo-vcs'',
		#parentIds : [
			''4d262680f3b602a16110a88251381d941f181e1e''
		]
	},
	HiExampleCommit {
		#id : ''4d262680f3b602a16110a88251381d941f181e1e'',
		#comment : ''second try on save added packages: if it is not exported, make the diff agains an empty snapshot.'',
		#parentIds : [
			''cba325a66eef2134022c8e0c32e59fe2149275db''
		]
	},
	HiExampleCommit {
		#id : ''535238f86f7b785bb0e07b9d0782989a47f3f790'',
		#comment : ''* added optional slash prefix to URL pattern in IceScpRemote>>parseUrl\n'',
		#parentIds : [
			''5382135a1ba5ca0bf64acd56055eec2d1b02b5b7''
		]
	},
	HiExampleCommit {
		#id : ''cba325a66eef2134022c8e0c32e59fe2149275db'',
		#comment : ''- add package now flushes is and marks as dirty (so we can save it).\n- add "remove from repository (do not unload)" option.\n- s/npasserini/pharo-vcs (but there is one remaining case)'',
		#parentIds : [
			''bb28858b4217e6ffda181ee482024e4cd12b95de''
		]
	},
	HiExampleCommit {
		#id : ''bb28858b4217e6ffda181ee482024e4cd12b95de'',
		#comment : ''- added compatibility methods to make Versionner work\n- added PR to pharo plugin list.'',
		#parentIds : [
			''4326fd48dfe33e8d676054534fc5dc799b9c298c''
		]
	},
	HiExampleCommit {
		#id : ''5382135a1ba5ca0bf64acd56055eec2d1b02b5b7'',
		#comment : ''Merge with 4326fd48dfe33e8d676054534fc5dc799b9c298c'',
		#parentIds : [
			''127aa13a598efd18cd6fa79068a700b08832f810'',
			''4326fd48dfe33e8d676054534fc5dc799b9c298c''
		]
	},
	HiExampleCommit {
		#id : ''4326fd48dfe33e8d676054534fc5dc799b9c298c'',
		#comment : ''add comments and fixing some minor details'',
		#parentIds : [
			''e0368664b730e12b26a63d47a75e4a3f5e181542''
		]
	},
	HiExampleCommit {
		#id : ''127aa13a598efd18cd6fa79068a700b08832f810'',
		#comment : ''Merge with e0368664b730e12b26a63d47a75e4a3f5e181542'',
		#parentIds : [
			''24065627837ecf220461d21ad1a1ab73c67e3263'',
			''e0368664b730e12b26a63d47a75e4a3f5e181542''
		]
	},
	HiExampleCommit {
		#id : ''e0368664b730e12b26a63d47a75e4a3f5e181542'',
		#comment : ''fix overrides'',
		#parentIds : [
			''65169ee36b88923c07ce01b4684f763fc5ac7075''
		]
	},
	HiExampleCommit {
		#id : ''24065627837ecf220461d21ad1a1ab73c67e3263'',
		#comment : ''Merge with 65169ee36b88923c07ce01b4684f763fc5ac7075'',
		#parentIds : [
			''374ffae3c0f951744fe9bb982d0f8a03c492b56d'',
			''65169ee36b88923c07ce01b4684f763fc5ac7075''
		]
	},
	HiExampleCommit {
		#id : ''374ffae3c0f951744fe9bb982d0f8a03c492b56d'',
		#comment : ''Merge pull request #342 from jecisc/cf_baseline_correction\n\nUse pharo-vcs instead of npasserini'',
		#parentIds : [
			''aedf916353c550f047b9fa6ef94e82dd6f651f6f'',
			''6729d83bc64742cca7ec32cae2c5aebb7d51b1e8''
		]
	},
	HiExampleCommit {
		#id : ''6729d83bc64742cca7ec32cae2c5aebb7d51b1e8'',
		#comment : ''Use pharo-vcs instead of npasserini\n\n'',
		#parentIds : [
			''aedf916353c550f047b9fa6ef94e82dd6f651f6f''
		]
	},
	HiExampleCommit {
		#id : ''aedf916353c550f047b9fa6ef94e82dd6f651f6f'',
		#comment : ''merge with dev-0.4'',
		#parentIds : [
			''0d85cebd06afcc96b55926d878b870404f330487''
		]
	},
	HiExampleCommit {
		#id : ''65169ee36b88923c07ce01b4684f763fc5ac7075'',
		#comment : ''Merging with c121d0b9fc30ae2f8c6520f9792b16970ef1e3fe'',
		#parentIds : [
			''8ecb0440292d3e3cb48195a18e7ae7fa210e263b'',
			''c121d0b9fc30ae2f8c6520f9792b16970ef1e3fe''
		]
	},
	HiExampleCommit {
		#id : ''0d85cebd06afcc96b55926d878b870404f330487'',
		#comment : ''using "master" as stable'',
		#parentIds : [
			''8ecb0440292d3e3cb48195a18e7ae7fa210e263b''
		]
	},
	HiExampleCommit {
		#id : ''064371e65ec2384a0cb94c3beef581af44e10b65'',
		#comment : ''Merging with 8ecb0440292d3e3cb48195a18e7ae7fa210e263b'',
		#parentIds : [
			''9e6ba4c9c503301660dc3f8fc9cc0214eb8ec779'',
			''8ecb0440292d3e3cb48195a18e7ae7fa210e263b''
		]
	},
	HiExampleCommit {
		#id : ''8ecb0440292d3e3cb48195a18e7ae7fa210e263b'',
		#comment : ''Force defaultBackendType to what I already know it will be the default. \nThis is not the best, but  it will work for now.'',
		#parentIds : [
			''e116878423277c0da57aa9090211159c2e810d2a''
		]
	},
	HiExampleCommit {
		#id : ''e116878423277c0da57aa9090211159c2e810d2a'',
		#comment : ''force DefaultBackendType to be what I know will be the default. \nI don\''t like this solution, but it will work.'',
		#parentIds : [
			''236b603b99f7e6b3045739994a9b7576276ee8d5''
		]
	},
	HiExampleCommit {
		#id : ''236b603b99f7e6b3045739994a9b7576276ee8d5'',
		#comment : ''added missing methods'',
		#parentIds : [
			''ce5d1c079f13d71d9eab6bfd71579fe50c28a158''
		]
	},
	HiExampleCommit {
		#id : ''ce5d1c079f13d71d9eab6bfd71579fe50c28a158'',
		#comment : ''add missing README.md\n'',
		#parentIds : [
			''e21bd9d9d8b2ab1d91d76b738d1668470e2cb6c2''
		]
	},
	HiExampleCommit {
		#id : ''e21bd9d9d8b2ab1d91d76b738d1668470e2cb6c2'',
		#comment : ''Merge branch \''master\'' into dev-0.4\n'',
		#parentIds : [
			''f7b5768590504159548a837cba260f55653a6c10'',
			''39e8bfb2f31f2065d59f0f9f99f226d57da0b735''
		]
	},
	HiExampleCommit {
		#id : ''f7b5768590504159548a837cba260f55653a6c10'',
		#comment : ''fix a problem with backend initialization'',
		#parentIds : [
			''3441a2926f03b3233fbd9359d1dafda78a259048''
		]
	},
	HiExampleCommit {
		#id : ''9e6ba4c9c503301660dc3f8fc9cc0214eb8ec779'',
		#comment : ''more on cherry pick (WIP)'',
		#parentIds : [
			''a983e01dc96bb6d0a7ba6163312b2290b8064790''
		]
	},
	HiExampleCommit {
		#id : ''a983e01dc96bb6d0a7ba6163312b2290b8064790'',
		#comment : ''another iteration to cherry peak. #allSelected option working'',
		#parentIds : [
			''28e0e304106d96c54d3fe679fec403802aab2711''
		]
	},
	HiExampleCommit {
		#id : ''28e0e304106d96c54d3fe679fec403802aab2711'',
		#comment : ''working on cherry pick (WIP)'',
		#parentIds : [
			''c121d0b9fc30ae2f8c6520f9792b16970ef1e3fe''
		]
	},
	HiExampleCommit {
		#id : ''c121d0b9fc30ae2f8c6520f9792b16970ef1e3fe'',
		#comment : ''fix fork name\n'',
		#parentIds : [
			''947ee35e68508af7ee6670d365edaa449bb7742c''
		]
	},
	HiExampleCommit {
		#id : ''947ee35e68508af7ee6670d365edaa449bb7742c'',
		#comment : ''Merge with 3441a2926f03b3233fbd9359d1dafda78a259048'',
		#parentIds : [
			''999ebab30bdce9e7e407b919130eda3149e22860'',
			''3441a2926f03b3233fbd9359d1dafda78a259048''
		]
	},
	HiExampleCommit {
		#id : ''999ebab30bdce9e7e407b919130eda3149e22860'',
		#comment : ''define env vars as global\n'',
		#parentIds : [
			''07cbf0146d9ced66f2814a7c4111a0a810592f3b''
		]
	},
	HiExampleCommit {
		#id : ''07cbf0146d9ced66f2814a7c4111a0a810592f3b'',
		#comment : ''add smalltalk_edge, to use my own fork (temporal)\n'',
		#parentIds : [
			''78238f05bc49d039114bac21c78788c2c0837f63''
		]
	},
	HiExampleCommit {
		#id : ''78238f05bc49d039114bac21c78788c2c0837f63'',
		#comment : ''add missing GLMSwappedColorDiffPresentation'',
		#parentIds : [
			''a8059a5b13c8c4ea865c2daf3001b9734e05cec4''
		]
	},
	HiExampleCommit {
		#id : ''a8059a5b13c8c4ea865c2daf3001b9734e05cec4'',
		#comment : ''Merge with refs/remotes/origin/dev-0.5'',
		#parentIds : [
			''b184bd7e13d91289fdd7c0d8a04fa349b7f39e97'',
			''85bb779eea6d1fc68c0065e493d22e1b295b1a6b''
		]
	},
	HiExampleCommit {
		#id : ''b184bd7e13d91289fdd7c0d8a04fa349b7f39e97'',
		#comment : ''Merge branch \''master\'' of github.com:pharo-vcs/iceberg into dev-0.5\n'',
		#parentIds : [
			''0088fdfbc4a8427f0bd1067e661b9df8e9155f94'',
			''39e8bfb2f31f2065d59f0f9f99f226d57da0b735''
		]
	},
	HiExampleCommit {
		#id : ''0088fdfbc4a8427f0bd1067e661b9df8e9155f94'',
		#comment : ''Merge branch \''master\'' into dev-0.5\n'',
		#parentIds : [
			''85bb779eea6d1fc68c0065e493d22e1b295b1a6b'',
			''421a8a88a9dce1f054dd8dd7dc4b88d59b1729e3''
		]
	},
	HiExampleCommit {
		#id : ''85bb779eea6d1fc68c0065e493d22e1b295b1a6b'',
		#comment : ''projectName does not have to contain ".git" suffix.'',
		#parentIds : [
			''9fd4ef2f61b49cd5c945aecb7cab343446a53d82''
		]
	},
	HiExampleCommit {
		#id : ''39e8bfb2f31f2065d59f0f9f99f226d57da0b735'',
		#comment : ''s/npasserini/pharo-vcs'',
		#parentIds : [
			''421a8a88a9dce1f054dd8dd7dc4b88d59b1729e3''
		]
	},
	HiExampleCommit {
		#id : ''9fd4ef2f61b49cd5c945aecb7cab343446a53d82'',
		#comment : ''marking testAddTag and testRemoveTag as expected failures because I think this is not incorrect that it fails (because they are empty, then not considered). \nI need to check later.'',
		#parentIds : [
			''e5dc32d4c9da49a7a3329dcf5e1157f035fe4c70''
		]
	},
	HiExampleCommit {
		#id : ''e5dc32d4c9da49a7a3329dcf5e1157f035fe4c70'',
		#comment : ''fix a problem when taking project name from url (it url had ".git" at the end, it was not properly taken, keeping the name there)\ns/npasserini/pharo-vcs '',
		#parentIds : [
			''d457a76a74444fe5dab997153b58c61be31c1d13''
		]
	},
	HiExampleCommit {
		#id : ''d457a76a74444fe5dab997153b58c61be31c1d13'',
		#comment : ''on conflict of test, use incomming sources (to ensure no changes affect the test)'',
		#parentIds : [
			''77577b32c1354bef7aaf2a6499daecd0e3e93dc8''
		]
	},
	HiExampleCommit {
		#id : ''77577b32c1354bef7aaf2a6499daecd0e3e93dc8'',
		#comment : ''using pharo-vcs for test-project source'',
		#parentIds : [
			''8c4d39ded1750ec6a51d46a5c9ec332a787cd418''
		]
	},
	HiExampleCommit {
		#id : ''8c4d39ded1750ec6a51d46a5c9ec332a787cd418'',
		#comment : ''Merge with fe61bbca5675e937f604b5bc25e3435ab6b1cbbd'',
		#parentIds : [
			''e557b5eb17968fa3d09969d65d63c71178159b8f'',
			''fe61bbca5675e937f604b5bc25e3435ab6b1cbbd''
		]
	},
	HiExampleCommit {
		#id : ''fe61bbca5675e937f604b5bc25e3435ab6b1cbbd'',
		#comment : ''Merge pull request #339 from peteruhnak/dev-0.5-travis-config-fix\n\n* commented-out ssh-key loading (it wasn\''t used)'',
		#parentIds : [
			''c582e87f9f84954dfe966cf57b5c1f62d482cbce'',
			''6142b28eca2ac9f449e1e8a630aef8aa8833ab28''
		]
	},
	HiExampleCommit {
		#id : ''e557b5eb17968fa3d09969d65d63c71178159b8f'',
		#comment : ''using https url instead ssh (because otherwise it fails). No idea if this will be like that at the end, but for now I let it there.'',
		#parentIds : [
			''bc1a05f558020f23989ff263c65022b4b63dda60''
		]
	},
	HiExampleCommit {
		#id : ''bc1a05f558020f23989ff263c65022b4b63dda60'',
		#comment : ''fixed assign of url (.git or not)'',
		#parentIds : [
			''ec130dc072eba80432ca9ebc768d3531a128d5ba''
		]
	},
	HiExampleCommit {
		#id : ''ec130dc072eba80432ca9ebc768d3531a128d5ba'',
		#comment : ''Merge with 84042fab4c1a62c3a762468809e53ccd5b06e531'',
		#parentIds : [
			''c582e87f9f84954dfe966cf57b5c1f62d482cbce'',
			''84042fab4c1a62c3a762468809e53ccd5b06e531''
		]
	},
	HiExampleCommit {
		#id : ''3441a2926f03b3233fbd9359d1dafda78a259048'',
		#comment : ''added a ConfigurationOf (to add to catalog. It will disappear in the future)'',
		#parentIds : [
			''84042fab4c1a62c3a762468809e53ccd5b06e531''
		]
	},
	HiExampleCommit {
		#id : ''6142b28eca2ac9f449e1e8a630aef8aa8833ab28'',
		#comment : ''* commented-out ssh-key loading (it wasn\''t used)\n* removed Pharo-5 (which Iceberg doesn\''t support)\n* Changed Pharo-6 to Pharo-alpha, because stable VM doesn\''t support\niceberg\''s git\n'',
		#parentIds : [
			''c582e87f9f84954dfe966cf57b5c1f62d482cbce''
		]
	},
	HiExampleCommit {
		#id : ''c582e87f9f84954dfe966cf57b5c1f62d482cbce'',
		#comment : ''introduce IceFullDiff to separate syncronisation from history view (which compares all packages).'',
		#parentIds : [
			''9b3a98b5767aa9001ff06f0c0a9e1f431c537270''
		]
	},
	HiExampleCommit {
		#id : ''9b3a98b5767aa9001ff06f0c0a9e1f431c537270'',
		#comment : ''- show differences just in modified packages. This will break some function so this is WIP :)\n- small refactor because I don\''t like so many ifs :P'',
		#parentIds : [
			''d5b3b93427074e9a5780965cc2f40467e6f0f82f''
		]
	},
	HiExampleCommit {
		#id : ''d5b3b93427074e9a5780965cc2f40467e6f0f82f'',
		#comment : ''baseline will use libgit2 branch v0.25.1'',
		#parentIds : [
			''84042fab4c1a62c3a762468809e53ccd5b06e531''
		]
	},
	HiExampleCommit {
		#id : ''421a8a88a9dce1f054dd8dd7dc4b88d59b1729e3'',
		#comment : ''Merge branch \''dev-0.4\''\n'',
		#parentIds : [
			''aa51ea5b6e1c71d1b64e2fe46a36a84478d7d140'',
			''bef06bb7bffee75d2d0eaf3acffe836f35652f45''
		]
	},
	HiExampleCommit {
		#id : ''bef06bb7bffee75d2d0eaf3acffe836f35652f45'',
		#comment : ''Merge branch \''master\'' into dev-0.4\n\n# Conflicts:\n#\tIceberg-UI.package/IceCreateRepositoryModel.class/properties.json\n#\tIceberg-UI.package/IceGlamourChangesTree.class/properties.json\n#\tIceberg-UI.package/IceGlamourSynchronizer.class/properties.json\n'',
		#parentIds : [
			''84042fab4c1a62c3a762468809e53ccd5b06e531'',
			''aa51ea5b6e1c71d1b64e2fe46a36a84478d7d140''
		]
	},
	HiExampleCommit {
		#id : ''aa51ea5b6e1c71d1b64e2fe46a36a84478d7d140'',
		#comment : ''update pre-requistites'',
		#parentIds : [
			''34fa836b0d0ba4578b6d8a1e97a753deaba871e0''
		]
	},
	HiExampleCommit {
		#id : ''34fa836b0d0ba4578b6d8a1e97a753deaba871e0'',
		#comment : ''update install instructions to avoid confusion'',
		#parentIds : [
			''8b6fa4a90e8933bb2b0ae26385bc60052cdd9c95''
		]
	},
	HiExampleCommit {
		#id : ''84042fab4c1a62c3a762468809e53ccd5b06e531'',
		#comment : ''ensure we compare by absolute reference because otherwise it missunderstoods equivallence between ./something and his absolute reference.'',
		#parentIds : [
			''97aa07a9a83ed1a33b5c281fed3ed28a61a00271''
		]
	},
	HiExampleCommit {
		#id : ''97aa07a9a83ed1a33b5c281fed3ed28a61a00271'',
		#comment : ''Merge pull request #333 from npasserini/performance-improvements\n\nPerformance improvements'',
		#parentIds : [
			''e034a089dc3e8c216a2aa6670cee8febc86648e0'',
			''a72c69fc5042c8c67978c59b190f65f5a4d852bd''
		]
	},
	HiExampleCommit {
		#id : ''a72c69fc5042c8c67978c59b190f65f5a4d852bd'',
		#comment : ''Do not consider commits from other repositories when computing reference commit.'',
		#parentIds : [
			''8c99c4f7e466548dd291956576329074ad8cfeaf''
		]
	},
	HiExampleCommit {
		#id : ''bd937b7565541a4fa4e801a817cf24b45dfa391e'',
		#comment : ''safer check of empty selection of packages '',
		#parentIds : [
			''1bb206206e3dc7c1a36dd29cc8918533c088d035''
		]
	},
	HiExampleCommit {
		#id : ''e034a089dc3e8c216a2aa6670cee8febc86648e0'',
		#comment : ''Merge pull request #328 from pavel-krivanek/issue304\n\nissue304'',
		#parentIds : [
			''1bb206206e3dc7c1a36dd29cc8918533c088d035'',
			''35eb9314309642a176fabc7d22e32bfd3d6cdc5d''
		]
	},
	HiExampleCommit {
		#id : ''8c99c4f7e466548dd291956576329074ad8cfeaf'',
		#comment : ''Avoid searching for VersionInfo through the history of commits/versions in the repository.\nCurrent implementation is not capable of really founding versions, trying to fix it does not seem easy and even if we could its utility is not guaranteed because it has severe performance issues for big repositories.\n'',
		#parentIds : [
			''b7d5453e5bc7d0812aca1fdba49c3b3da6a3ae94''
		]
	},
	HiExampleCommit {
		#id : ''35eb9314309642a176fabc7d22e32bfd3d6cdc5d'',
		#comment : ''- refresh repositories list after branch switch'',
		#parentIds : [
			''1bb206206e3dc7c1a36dd29cc8918533c088d035''
		]
	},
	HiExampleCommit {
		#id : ''1bb206206e3dc7c1a36dd29cc8918533c088d035'',
		#comment : ''fix validate (use remoteToUse instead url)'',
		#parentIds : [
			''4d2cad0d7f30bf0e50f36cc3a6949457ee154a7c''
		]
	},
	HiExampleCommit {
		#id : ''b7d5453e5bc7d0812aca1fdba49c3b3da6a3ae94'',
		#comment : ''Avoid collecting all past versions of a package when trying to compute current one.'',
		#parentIds : [
			''4d2cad0d7f30bf0e50f36cc3a6949457ee154a7c''
		]
	},
	HiExampleCommit {
		#id : ''8b6fa4a90e8933bb2b0ae26385bc60052cdd9c95'',
		#comment : ''Merge pull request #324 from Uko/master\n\nSwapped colors, so additions in Iceberg are green and removals are red'',
		#parentIds : [
			''c3e4bc06c024b3aeb44b89cb6767808c5278a83b'',
			''21f71e4180fb220446e1f2e30efa4d23083431bc''
		]
	},
	HiExampleCommit {
		#id : ''4d2cad0d7f30bf0e50f36cc3a6949457ee154a7c'',
		#comment : ''fixes #323\n'',
		#parentIds : [
			''c8d52f1d2f05b56866172417f4b04245b62a4bae''
		]
	},
	HiExampleCommit {
		#id : ''21f71e4180fb220446e1f2e30efa4d23083431bc'',
		#comment : ''Added a diff morph with swapped colors, so additions in Iceberg are green and removals are red'',
		#parentIds : [
			''c3e4bc06c024b3aeb44b89cb6767808c5278a83b''
		]
	},
	HiExampleCommit {
		#id : ''c8d52f1d2f05b56866172417f4b04245b62a4bae'',
		#comment : ''add "install baseline" plugin :)'',
		#parentIds : [
			''b8cd5320a1263a5e1437596f925ee2301a11bfa4''
		]
	},
	HiExampleCommit {
		#id : ''b8cd5320a1263a5e1437596f925ee2301a11bfa4'',
		#comment : ''fix location of repositories (it was never found)\nadd first iteration of "install baseline" (groups missing)'',
		#parentIds : [
			''c06005523fdebf0faa49c8b72d8174b1624dd29e''
		]
	},
	HiExampleCommit {
		#id : ''c06005523fdebf0faa49c8b72d8174b1624dd29e'',
		#comment : ''Merge pull request #322 from pavel-krivanek/issue321\n\nissue321'',
		#parentIds : [
			''02b1b66192f97fed9108f5ae50cbd8d2b3c498ef'',
			''23c41491848019c007c6c23c60f1f8d01dad4edd''
		]
	},
	HiExampleCommit {
		#id : ''23c41491848019c007c6c23c60f1f8d01dad4edd'',
		#comment : ''weak repository subscriptions'',
		#parentIds : [
			''5e5d52d996801802ebc22930d73d16e25979d362''
		]
	},
	HiExampleCommit {
		#id : ''5e5d52d996801802ebc22930d73d16e25979d362'',
		#comment : ''make repository announcer subscriptions weak'',
		#parentIds : [
			''524628559b030b3d2fc19c716fe89f4d563f22bb''
		]
	},
	HiExampleCommit {
		#id : ''524628559b030b3d2fc19c716fe89f4d563f22bb'',
		#comment : ''next mixed type of announcement subscription'',
		#parentIds : [
			''56ec380c208dcc1135ae1e7050ab49253332f28f''
		]
	},
	HiExampleCommit {
		#id : ''56ec380c208dcc1135ae1e7050ab49253332f28f'',
		#comment : ''revert non-subscribing code'',
		#parentIds : [
			''801617ef79900d0c5b04b659f565c8ee0e3e6d82''
		]
	},
	HiExampleCommit {
		#id : ''801617ef79900d0c5b04b659f565c8ee0e3e6d82'',
		#comment : ''- use weak announcements for the Iceberg announcer\n- use strong announcements for local announcers'',
		#parentIds : [
			''02b1b66192f97fed9108f5ae50cbd8d2b3c498ef''
		]
	},
	HiExampleCommit {
		#id : ''02b1b66192f97fed9108f5ae50cbd8d2b3c498ef'',
		#comment : ''typo'',
		#parentIds : [
			''17d696e1480983ad06e20b689fb42bd0dc76761e''
		]
	},
	HiExampleCommit {
		#id : ''17d696e1480983ad06e20b689fb42bd0dc76761e'',
		#comment : ''refactor action names of IceRepositoriesBrowser\ncleanup a bit IceRepositoryModel'',
		#parentIds : [
			''f95832db0c2a1063a684354295bd67d944676fa3''
		]
	},
	HiExampleCommit {
		#id : ''f95832db0c2a1063a684354295bd67d944676fa3'',
		#comment : ''add remove form disk. \rcloses #317'',
		#parentIds : [
			''47ae4860ab23c9e7e97f46323c5f91729d384e77''
		]
	},
	HiExampleCommit {
		#id : ''47ae4860ab23c9e7e97f46323c5f91729d384e77'',
		#comment : ''IceRepository>>#name needs to be initialised with project  basename, not with full name (because it can have a ".git" there)'',
		#parentIds : [
			''7601ac3c455f0fac636fb6dce547f2710552945d''
		]
	},
	HiExampleCommit {
		#id : ''7601ac3c455f0fac636fb6dce547f2710552945d'',
		#comment : ''Merge pull request #318 from pavel-krivanek/packagesReloading\n\nPackages reloading'',
		#parentIds : [
			''e63df40385d346856b76905811c56a28341d49f6'',
			''849eb3f71cf7d967d82526855b01c871622e1221''
		]
	},
	HiExampleCommit {
		#id : ''849eb3f71cf7d967d82526855b01c871622e1221'',
		#comment : ''add condition'',
		#parentIds : [
			''93a26a74433188636f609fdc6f959e4d894d6b5e''
		]
	},
	HiExampleCommit {
		#id : ''93a26a74433188636f609fdc6f959e4d894d6b5e'',
		#comment : ''reload all packages button'',
		#parentIds : [
			''d2c768f6c88c8e8f86ae59845e4ab19f59afdf88''
		]
	},
	HiExampleCommit {
		#id : ''d2c768f6c88c8e8f86ae59845e4ab19f59afdf88'',
		#comment : ''basic package reload action'',
		#parentIds : [
			''e63df40385d346856b76905811c56a28341d49f6''
		]
	},
	HiExampleCommit {
		#id : ''e63df40385d346856b76905811c56a28341d49f6'',
		#comment : ''added IceRemote>>#projectBasename to get the correct "base" name (without decorations style ".git") of a remote. '',
		#parentIds : [
			''a08c09f3898858070d361448ee67c2df7938b29a''
		]
	},
	HiExampleCommit {
		#id : ''a08c09f3898858070d361448ee67c2df7938b29a'',
		#comment : ''pullRemote/pushRemote can be nil\r'',
		#parentIds : [
			''262897a7a491b308cc93b9849ccbf46a4903065e''
		]
	},
	HiExampleCommit {
		#id : ''262897a7a491b308cc93b9849ccbf46a4903065e'',
		#comment : ''thing is, IceDiff can have a theirVersion who is nil, in which case the change set will be empty. '',
		#parentIds : [
			''0e28fe35bc23228a9b45044212a158288ee2502f''
		]
	},
	HiExampleCommit {
		#id : ''0e28fe35bc23228a9b45044212a158288ee2502f'',
		#comment : ''- add possibility of create new repositories (from scratch)\r- refactor some GUI methods\r- revamp the #origin bugfix for issue #282'',
		#parentIds : [
			''3a0a1aacf1161219cb77b496813f36f33bd6918a''
		]
	},
	HiExampleCommit {
		#id : ''3a0a1aacf1161219cb77b496813f36f33bd6918a'',
		#comment : ''Merge pull request #315 from demarey/dev-0.4\n\nfix issue #282'',
		#parentIds : [
			''98f6c6c763d43f2d68f00037351af3d02db522e2'',
			''aaf2f44b47737e4a0542bfb59e43c5f9db581007''
		]
	},
	HiExampleCommit {
		#id : ''aaf2f44b47737e4a0542bfb59e43c5f9db581007'',
		#comment : ''clean code a bit'',
		#parentIds : [
			''b7d13ffaf1a50b8056457a38237ae62bee1da853''
		]
	},
	HiExampleCommit {
		#id : ''b7d13ffaf1a50b8056457a38237ae62bee1da853'',
		#comment : ''fix issue #292'',
		#parentIds : [
			''16333ea84830b4d471707a1d14d32bdd0e780695''
		]
	},
	HiExampleCommit {
		#id : ''98f6c6c763d43f2d68f00037351af3d02db522e2'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''39f91e1f47082d59963050f52a91116ed7f15b39'',
			''cebd870e38739c0fc1f71c8ee7afd81211973d6e''
		]
	},
	HiExampleCommit {
		#id : ''39f91e1f47082d59963050f52a91116ed7f15b39'',
		#comment : ''saving last status of "upload '',
		#parentIds : [
			''e35c3900af0afc635c90097d3519ec800aeca6b2''
		]
	},
	HiExampleCommit {
		#id : ''e35c3900af0afc635c90097d3519ec800aeca6b2'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''d72b099668de377354dce86d0ac4b8dae8a4dec1'',
			''cebd870e38739c0fc1f71c8ee7afd81211973d6e''
		]
	},
	HiExampleCommit {
		#id : ''d72b099668de377354dce86d0ac4b8dae8a4dec1'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''05dc40a4eb58ae4bb4e9eb5da1f056dc71391a7f'',
			''cebd870e38739c0fc1f71c8ee7afd81211973d6e''
		]
	},
	HiExampleCommit {
		#id : ''05dc40a4eb58ae4bb4e9eb5da1f056dc71391a7f'',
		#comment : ''add handling on user.name and user.email'',
		#parentIds : [
			''cebd870e38739c0fc1f71c8ee7afd81211973d6e''
		]
	},
	HiExampleCommit {
		#id : ''cebd870e38739c0fc1f71c8ee7afd81211973d6e'',
		#comment : ''Merge pull request #311 from demarey/issue-292\n\nfix Issue #292'',
		#parentIds : [
			''16333ea84830b4d471707a1d14d32bdd0e780695'',
			''76ed3a2f96b199036d8bb599e42096ad76351127''
		]
	},
	HiExampleCommit {
		#id : ''76ed3a2f96b199036d8bb599e42096ad76351127'',
		#comment : ''avoid errors when switching to a remote branch by creating a local branch if needed.\rDetached head mode is not yet supported.'',
		#parentIds : [
			''16333ea84830b4d471707a1d14d32bdd0e780695''
		]
	},
	HiExampleCommit {
		#id : ''16333ea84830b4d471707a1d14d32bdd0e780695'',
		#comment : ''remove laft halt'',
		#parentIds : [
			''edb9e8897c1fa9186b80f03dfc8fe1b644a91625''
		]
	},
	HiExampleCommit {
		#id : ''edb9e8897c1fa9186b80f03dfc8fe1b644a91625'',
		#comment : ''- add "compare branch with current sources" option in branches.\r- add IceBranchBrowser to show a branch history (is same as history browser, but restricted to a branch)\r- a couple of refactors'',
		#parentIds : [
			''419bc1be3fb3f3bce4bbeabf6ad6dcde7b48d00b''
		]
	},
	HiExampleCommit {
		#id : ''419bc1be3fb3f3bce4bbeabf6ad6dcde7b48d00b'',
		#comment : ''Merge with 07db4f08f31da912d786e6bfce258da3f231e344'',
		#parentIds : [
			''17c9b8a8363068308ebc6b0ed540652b91656673'',
			''07db4f08f31da912d786e6bfce258da3f231e344''
		]
	},
	HiExampleCommit {
		#id : ''07db4f08f31da912d786e6bfce258da3f231e344'',
		#comment : ''Merge pull request #303 from estebanlm/dev-0.4\n\ncreate #compatibleUsername who will answer FirstnameLastname instead â\u0080¦'',
		#parentIds : [
			''97da77c9a865fabb9f13cbbdda85e136dd013ea4'',
			''bafb5b6a05fad5d9c15ae5a012ce69c7902c1d3f''
		]
	},
	HiExampleCommit {
		#id : ''17c9b8a8363068308ebc6b0ed540652b91656673'',
		#comment : ''Merge with 97da77c9a865fabb9f13cbbdda85e136dd013ea4'',
		#parentIds : [
			''ffdcb46a5c82f7fb3805d6625164559768fd4339'',
			''97da77c9a865fabb9f13cbbdda85e136dd013ea4''
		]
	},
	HiExampleCommit {
		#id : ''97da77c9a865fabb9f13cbbdda85e136dd013ea4'',
		#comment : ''Merge pull request #305 from astares/master\n\nExtended URL support for Remotes in Iceberg to solve issue 284'',
		#parentIds : [
			''295ad89ea501d0572ba6e8c3013632ab66bef334'',
			''e7d2e1382a21ac46fe91923739ebb8dfe7b5303c''
		]
	},
	HiExampleCommit {
		#id : ''ffdcb46a5c82f7fb3805d6625164559768fd4339'',
		#comment : ''Merge with 295ad89ea501d0572ba6e8c3013632ab66bef334'',
		#parentIds : [
			''bafb5b6a05fad5d9c15ae5a012ce69c7902c1d3f'',
			''295ad89ea501d0572ba6e8c3013632ab66bef334''
		]
	},
	HiExampleCommit {
		#id : ''295ad89ea501d0572ba6e8c3013632ab66bef334'',
		#comment : ''Merge pull request #300 from demarey/issue-299\n\nAvoid red morph when creating a repository with a wrong subdirectory.â\u0080¦'',
		#parentIds : [
			''e1470bfa2a8164ee593ed99c73f7aa59b8c3a0de'',
			''2eb9cfe84ea1e2141a0b54a53ee320c40ab1cd18''
		]
	},
	HiExampleCommit {
		#id : ''e7d2e1382a21ac46fe91923739ebb8dfe7b5303c'',
		#comment : ''Merge branch \''dev-0.4\'' into master'',
		#parentIds : [
			''d19686ad7a4a3e8b2aeca92f1ce994de3dd97f36'',
			''e1470bfa2a8164ee593ed99c73f7aa59b8c3a0de''
		]
	},
	HiExampleCommit {
		#id : ''2eb9cfe84ea1e2141a0b54a53ee320c40ab1cd18'',
		#comment : ''Avoid red morph when creating a repository with a wrong subdirectory.\rFix issue #299'',
		#parentIds : [
			''e1470bfa2a8164ee593ed99c73f7aa59b8c3a0de''
		]
	},
	HiExampleCommit {
		#id : ''bafb5b6a05fad5d9c15ae5a012ce69c7902c1d3f'',
		#comment : ''create #compatibleUsername who will answer FirstnameLastname instead \''Firstname Lastname\'''',
		#parentIds : [
			''e1470bfa2a8164ee593ed99c73f7aa59b8c3a0de''
		]
	},
	HiExampleCommit {
		#id : ''e1470bfa2a8164ee593ed99c73f7aa59b8c3a0de'',
		#comment : ''get browser to update correctly when editing a repository.'',
		#parentIds : [
			''8ea5bdf4c8dd84bced0f68b0389b5b40bf2bdb3b''
		]
	},
	HiExampleCommit {
		#id : ''8ea5bdf4c8dd84bced0f68b0389b5b40bf2bdb3b'',
		#comment : ''edit repository showing correct errors when not location/code is provided.\rI still cannot get browser to update correctly (no idea why, working on it).'',
		#parentIds : [
			''ad806911c4e31464c257d20fc9031cad5baa9762''
		]
	},
	HiExampleCommit {
		#id : ''ad806911c4e31464c257d20fc9031cad5baa9762'',
		#comment : ''add an edit repository dialog (WIP)'',
		#parentIds : [
			''114a889bb803da29654c69af66251e3e3a5d2678''
		]
	},
	HiExampleCommit {
		#id : ''114a889bb803da29654c69af66251e3e3a5d2678'',
		#comment : ''taking into account defined headRemote to get repository data'',
		#parentIds : [
			''8b75d92ee4c7e351952c8d0fd54bcf056af0c8f3''
		]
	},
	HiExampleCommit {
		#id : ''8b75d92ee4c7e351952c8d0fd54bcf056af0c8f3'',
		#comment : ''add "create pull request" option to remotes.'',
		#parentIds : [
			''813fffb4a102f65659745f96c3ec5b09f4ec1308''
		]
	},
	HiExampleCommit {
		#id : ''813fffb4a102f65659745f96c3ec5b09f4ec1308'',
		#comment : ''Merge with d648b516ab9008f2b29166b6173a743bb79a00ba'',
		#parentIds : [
			''8f394f4e8b2820335ecff3cf0ad60448889d393b'',
			''d648b516ab9008f2b29166b6173a743bb79a00ba''
		]
	},
	HiExampleCommit {
		#id : ''d648b516ab9008f2b29166b6173a743bb79a00ba'',
		#comment : ''fix a test using #subdirectory: on an IceRepository'',
		#parentIds : [
			''fc925716a3f5b99ace8f207c53071cada6ff4251''
		]
	},
	HiExampleCommit {
		#id : ''fc925716a3f5b99ace8f207c53071cada6ff4251'',
		#comment : ''Use subdirectory accessor as i.v. was moved to backend'',
		#parentIds : [
			''33a00e6ebca589ebcd345ef424b672ebaf416678''
		]
	},
	HiExampleCommit {
		#id : ''33a00e6ebca589ebcd345ef424b672ebaf416678'',
		#comment : ''fix wrong block closing'',
		#parentIds : [
			''32efc2ac2bf86c06bbc232aea7ece42128d8ccd4''
		]
	},
	HiExampleCommit {
		#id : ''d19686ad7a4a3e8b2aeca92f1ce994de3dd97f36'',
		#comment : ''Allow "-" in owner and repo name\n'',
		#parentIds : [
			''2b69e6461ea8dd4cedaad0707f771eed3c5eb98b''
		]
	},
	HiExampleCommit {
		#id : ''2b69e6461ea8dd4cedaad0707f771eed3c5eb98b'',
		#comment : ''Add support for more possible URLs (including different port, subpaths, HTTPS and HTTP, login user, ...) together with full set of tests'',
		#parentIds : [
			''cca6b4926bc2e969f02fa257db891270828d111f''
		]
	},
	HiExampleCommit {
		#id : ''cca6b4926bc2e969f02fa257db891270828d111f'',
		#comment : ''Merge branch \''dev-0.4\'' of https://github.com/npasserini/iceberg\n'',
		#parentIds : [
			''c3e4bc06c024b3aeb44b89cb6767808c5278a83b'',
			''8f394f4e8b2820335ecff3cf0ad60448889d393b''
		]
	},
	HiExampleCommit {
		#id : ''8f394f4e8b2820335ecff3cf0ad60448889d393b'',
		#comment : ''Merge pull request #277 from estebanlm/plugin-pharo-fogbugz\n\nplugin-pharo-fogbugz'',
		#parentIds : [
			''a426fc60545a3d40b4ed78716de68fba724f9b2d'',
			''e437e9b46a4354b102125abf9a4f7032d5b5096b''
		]
	},
	HiExampleCommit {
		#id : ''a426fc60545a3d40b4ed78716de68fba724f9b2d'',
		#comment : ''Merge pull request #280 from estebanlm/dev-0.4\n\nin diff trees show just packages with changes'',
		#parentIds : [
			''e64d19f5a624dbcdc7626c89bd10b64e86c3b3b3'',
			''c3e96d9b4e8d804af0a97c8acc1a535867fd0ad9''
		]
	},
	HiExampleCommit {
		#id : ''32efc2ac2bf86c06bbc232aea7ece42128d8ccd4'',
		#comment : ''syntax error\n'',
		#parentIds : [
			''66236f3caf108c4ea237e486d008ee62055f3576''
		]
	},
	HiExampleCommit {
		#id : ''66236f3caf108c4ea237e486d008ee62055f3576'',
		#comment : ''merge with core repo\n'',
		#parentIds : [
			''bb8bdd0cd14b3ea4d625139fee3133b0cbd48690'',
			''e64d19f5a624dbcdc7626c89bd10b64e86c3b3b3''
		]
	},
	HiExampleCommit {
		#id : ''e437e9b46a4354b102125abf9a4f7032d5b5096b'',
		#comment : ''Merge with refs/remotes/estebanlm/plugin-pharo-fogbugz'',
		#parentIds : [
			''4ae83128c61e498a4111e213c3bd6edfb274941a'',
			''c04a08dcdd3f37b627ca221daded7ade553cf524''
		]
	},
	HiExampleCommit {
		#id : ''4ae83128c61e498a4111e213c3bd6edfb274941a'',
		#comment : ''- start a refactor of github plugin actions to a command hierarchy\r- add "remove old branchs" command (which includes an UI and some api additions)\r-  add "remote plugin actions" capability to repositories browser.'',
		#parentIds : [
			''45768b79a76425ce792a88159237fd059f3efd97''
		]
	},
	HiExampleCommit {
		#id : ''c3e96d9b4e8d804af0a97c8acc1a535867fd0ad9'',
		#comment : ''showing just packages with changes in trees '',
		#parentIds : [
			''70f620e2677bdea88419bb274cb4a9ef770aa1c4''
		]
	},
	HiExampleCommit {
		#id : ''70f620e2677bdea88419bb274cb4a9ef770aa1c4'',
		#comment : ''Merge with e64d19f5a624dbcdc7626c89bd10b64e86c3b3b3'',
		#parentIds : [
			''3c99b34a0eee8dadc8cc66796cf523581550ef51'',
			''e64d19f5a624dbcdc7626c89bd10b64e86c3b3b3''
		]
	},
	HiExampleCommit {
		#id : ''c04a08dcdd3f37b627ca221daded7ade553cf524'',
		#comment : ''fix method name'',
		#parentIds : [
			''45768b79a76425ce792a88159237fd059f3efd97''
		]
	},
	HiExampleCommit {
		#id : ''45768b79a76425ce792a88159237fd059f3efd97'',
		#comment : ''Merge with 3c99b34a0eee8dadc8cc66796cf523581550ef51'',
		#parentIds : [
			''f37e0dea62d75930cb47772238433d127667cd25'',
			''3c99b34a0eee8dadc8cc66796cf523581550ef51''
		]
	},
	HiExampleCommit {
		#id : ''f37e0dea62d75930cb47772238433d127667cd25'',
		#comment : ''rename some methods '',
		#parentIds : [
			''e47f93700c6aaabb56a6ed31c1435b0eb1027f47''
		]
	},
	HiExampleCommit {
		#id : ''e64d19f5a624dbcdc7626c89bd10b64e86c3b3b3'',
		#comment : ''Merge pull request #276 from estebanlm/dev-0.4\n\nenable use of ssh'',
		#parentIds : [
			''4ddee275bf0bf0e7911db271d55c26fe1eb214b2'',
			''c4c89cc75e873d890fb802a7f652c760347b75ec''
		]
	},
	HiExampleCommit {
		#id : ''e47f93700c6aaabb56a6ed31c1435b0eb1027f47'',
		#comment : ''- fix window delete on UI.\r- add branch info on repository detailed info.'',
		#parentIds : [
			''e6270518c27316bef32671bba6ee7e0eb3f2c633''
		]
	},
	HiExampleCommit {
		#id : ''e6270518c27316bef32671bba6ee7e0eb3f2c633'',
		#comment : ''enhance a bit the UI'',
		#parentIds : [
			''5bbfa172688efc3c0f1921d2db2258fdf4301a4a''
		]
	},
	HiExampleCommit {
		#id : ''5bbfa172688efc3c0f1921d2db2258fdf4301a4a'',
		#comment : ''add class comments and change a category'',
		#parentIds : [
			''f22dbb75452242de91c8756956b85519201b7cc0''
		]
	},
	HiExampleCommit {
		#id : ''f22dbb75452242de91c8756956b85519201b7cc0'',
		#comment : ''update panel after change'',
		#parentIds : [
			''6c49765d791370f1e99e43fa18a2abb5794efddb''
		]
	},
	HiExampleCommit {
		#id : ''6c49765d791370f1e99e43fa18a2abb5794efddb'',
		#comment : ''delete window after accept'',
		#parentIds : [
			''dee2fffbad0bb227b572289f2a16f3152c820cab''
		]
	},
	HiExampleCommit {
		#id : ''dee2fffbad0bb227b572289f2a16f3152c820cab'',
		#comment : ''first version'',
		#parentIds : [
			''3c99b34a0eee8dadc8cc66796cf523581550ef51''
		]
	},
	HiExampleCommit {
		#id : ''3c99b34a0eee8dadc8cc66796cf523581550ef51'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''e489e3ebb514a8b23fa9c4aed89d8da53525bd00'',
			''4ddee275bf0bf0e7911db271d55c26fe1eb214b2''
		]
	},
	HiExampleCommit {
		#id : ''c4c89cc75e873d890fb802a7f652c760347b75ec'',
		#comment : ''using correct url for clone (it was forcing httpsUrl because ssh was not working)'',
		#parentIds : [
			''e489e3ebb514a8b23fa9c4aed89d8da53525bd00''
		]
	},
	HiExampleCommit {
		#id : ''e489e3ebb514a8b23fa9c4aed89d8da53525bd00'',
		#comment : ''add validation of "location" before using it '',
		#parentIds : [
			''a4b72252139be1d2aa8818456d88c5fb345ddea8''
		]
	},
	HiExampleCommit {
		#id : ''a4b72252139be1d2aa8818456d88c5fb345ddea8'',
		#comment : ''added dialog to ask for ssh credentials\renhanced clone/add repository dialogs by using a common location model'',
		#parentIds : [
			''44e6bb2c60d765a428189ebe403d6303b85c0600''
		]
	},
	HiExampleCommit {
		#id : ''44e6bb2c60d765a428189ebe403d6303b85c0600'',
		#comment : ''adding keys to ssh-agent\n'',
		#parentIds : [
			''df7db4406f3620800a6ce6354def489fe48101c8''
		]
	},
	HiExampleCommit {
		#id : ''df7db4406f3620800a6ce6354def489fe48101c8'',
		#comment : ''using maging 3 number (that seems to work)'',
		#parentIds : [
			''e16205f68b5d3f7292f6514c89e557b491639d48''
		]
	},
	HiExampleCommit {
		#id : ''e16205f68b5d3f7292f6514c89e557b491639d48'',
		#comment : ''add settings for use ssh-agent or custom files'',
		#parentIds : [
			''6970b2dbc98b34b303967e6509cdcbed374239d8''
		]
	},
	HiExampleCommit {
		#id : ''6970b2dbc98b34b303967e6509cdcbed374239d8'',
		#comment : ''add guard to ensure ssh-agent failure actualy fails '',
		#parentIds : [
			''ea31fd82444988a0ef865fb4b6f8391166b463a7''
		]
	},
	HiExampleCommit {
		#id : ''ea31fd82444988a0ef865fb4b6f8391166b463a7'',
		#comment : ''clean dependencies with iceberg'',
		#parentIds : [
			''0fd352f6927cd86352e196671751e4b3c3d9c93b''
		]
	},
	HiExampleCommit {
		#id : ''0fd352f6927cd86352e196671751e4b3c3d9c93b'',
		#comment : ''add ssh-agent strategy'',
		#parentIds : [
			''572287a25c6ff615e6e2a37b2676eb5a8cd936b0''
		]
	},
	HiExampleCommit {
		#id : ''572287a25c6ff615e6e2a37b2676eb5a8cd936b0'',
		#comment : ''creating correct ssh keys (WIP)'',
		#parentIds : [
			''4ddee275bf0bf0e7911db271d55c26fe1eb214b2''
		]
	},
	HiExampleCommit {
		#id : ''4ddee275bf0bf0e7911db271d55c26fe1eb214b2'',
		#comment : ''Merge pull request #268 from estebanlm/dev-0.4\n\nfirst shot to plugins (and GitHub PR as test case)'',
		#parentIds : [
			''79f01afcbdc6201e30efaa82c8c962860a23013e'',
			''237aa21d28d0b9ab530eaff18819ec8f5fb32baf''
		]
	},
	HiExampleCommit {
		#id : ''237aa21d28d0b9ab530eaff18819ec8f5fb32baf'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''483dd146005a7cbf26f19a9cc0cf958db9f8d41a'',
			''79f01afcbdc6201e30efaa82c8c962860a23013e''
		]
	},
	HiExampleCommit {
		#id : ''483dd146005a7cbf26f19a9cc0cf958db9f8d41a'',
		#comment : ''enhanced dialog'',
		#parentIds : [
			''df745d2713f6b963f616cfe7f1e047a8926f4ce1''
		]
	},
	HiExampleCommit {
		#id : ''df745d2713f6b963f616cfe7f1e047a8926f4ce1'',
		#comment : ''add Iceberg-Plugin to BaselineOfIceberf'',
		#parentIds : [
			''a921b00dd39faa8a51f22e616db4da9cafc1067b''
		]
	},
	HiExampleCommit {
		#id : ''a921b00dd39faa8a51f22e616db4da9cafc1067b'',
		#comment : ''remove remaining variable'',
		#parentIds : [
			''65ead086fa968be0517bb76d318b84d3a4eebb93''
		]
	},
	HiExampleCommit {
		#id : ''65ead086fa968be0517bb76d318b84d3a4eebb93'',
		#comment : ''add a validation step to show when current sources do not correspond with the pull-request-to-be'',
		#parentIds : [
			''327c2ba2359e1605d555cd1a7c4353085fc4b76d''
		]
	},
	HiExampleCommit {
		#id : ''327c2ba2359e1605d555cd1a7c4353085fc4b76d'',
		#comment : ''- removed unused class (it was replaced for a builder)\r- a class comment'',
		#parentIds : [
			''4d4f1783fe4d8e90931b84d4e319bacb07b1cb91''
		]
	},
	HiExampleCommit {
		#id : ''4d4f1783fe4d8e90931b84d4e319bacb07b1cb91'',
		#comment : ''refactor a bit to extract GitHub API into it\''s own class'',
		#parentIds : [
			''331d2bc90abc904a939bf47abe79e3b563fc5e8f''
		]
	},
	HiExampleCommit {
		#id : ''331d2bc90abc904a939bf47abe79e3b563fc5e8f'',
		#comment : ''base remote is the pushRemote (which most of the cases is the same as origin, but not always)'',
		#parentIds : [
			''d6b192dc2219831e729da813d301b4a5dadeb89b''
		]
	},
	HiExampleCommit {
		#id : ''d6b192dc2219831e729da813d301b4a5dadeb89b'',
		#comment : ''a complete revamp to take advantage of github information .\rStill needs some polishing, but it works :)'',
		#parentIds : [
			''0e09a2862dddbf308960ce8c18fca709121ff3d4''
		]
	},
	HiExampleCommit {
		#id : ''0e09a2862dddbf308960ce8c18fca709121ff3d4'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''1c8926331ffaea549059f5d117a9f95dcdd52c0b'',
			''79f01afcbdc6201e30efaa82c8c962860a23013e''
		]
	},
	HiExampleCommit {
		#id : ''1c8926331ffaea549059f5d117a9f95dcdd52c0b'',
		#comment : ''recategorised'',
		#parentIds : [
			''4e7ad568961b965274582afc93b27e3eb6118b9e''
		]
	},
	HiExampleCommit {
		#id : ''4e7ad568961b965274582afc93b27e3eb6118b9e'',
		#comment : ''added missing package :P'',
		#parentIds : [
			''57755daa9a2b35b5a76686104f52fa03dbdd0744''
		]
	},
	HiExampleCommit {
		#id : ''57755daa9a2b35b5a76686104f52fa03dbdd0744'',
		#comment : ''first implementation with "GitHub - New Pull Request" as case'',
		#parentIds : [
			''79f01afcbdc6201e30efaa82c8c962860a23013e''
		]
	},
	HiExampleCommit {
		#id : ''79f01afcbdc6201e30efaa82c8c962860a23013e'',
		#comment : ''revert some  left-overs from another branch'',
		#parentIds : [
			''bf887a0a323f28cf06b0da37c1321908b75f73a3''
		]
	},
	HiExampleCommit {
		#id : ''bf887a0a323f28cf06b0da37c1321908b75f73a3'',
		#comment : ''passing entity to IceDiffChangeTreeBuilder, otherwise it does not takes correcting values to evaluate actions.'',
		#parentIds : [
			''eba2509c192ac431494b800feab057135c1c6758''
		]
	},
	HiExampleCommit {
		#id : ''eba2509c192ac431494b800feab057135c1c6758'',
		#comment : ''add explanation '',
		#parentIds : [
			''9653d753963eb89b155db577fa2a59760cfbabb9''
		]
	},
	HiExampleCommit {
		#id : ''9653d753963eb89b155db577fa2a59760cfbabb9'',
		#comment : ''fix an error that happens when a repository is new and it has no branches  (aka an "unborn" branch): we need to create one by default (for now, it will be "master").'',
		#parentIds : [
			''d8559ebba4090f785c8a235100829c2e45f80fa4''
		]
	},
	HiExampleCommit {
		#id : ''d8559ebba4090f785c8a235100829c2e45f80fa4'',
		#comment : ''- changed the way trees are shown (they  are more efficient now, and they remember last selection so is easier to navigate on commits). Now we created `IceDiffChangeTreeBuilder` to build the element, instead a composite wrapped.\r- changed detection of default backend to default to libgit2 backend.\r- restore tests on all backends (waiting for promote to stable latest vm).'',
		#parentIds : [
			''eff878a510796a32bebc9826d6bb93151fc6003b''
		]
	},
	HiExampleCommit {
		#id : ''eff878a510796a32bebc9826d6bb93151fc6003b'',
		#comment : ''#pushTo: needs to set the upstream to "remoteName/banchName" if there is no branch (it was setting origin/branchName), and that causes an error.'',
		#parentIds : [
			''3ebdcfb55770e5e70e2270bbf2574da4438da4ef''
		]
	},
	HiExampleCommit {
		#id : ''3ebdcfb55770e5e70e2270bbf2574da4438da4ef'',
		#comment : ''if there is no upstream for a remote (which would mean branch is not in remote), answer an empty array (it will show all commits as "not pushed".'',
		#parentIds : [
			''e40bdd03b0367c75f7c8a45803cff6c26a860b63''
		]
	},
	HiExampleCommit {
		#id : ''c3e4bc06c024b3aeb44b89cb6767808c5278a83b'',
		#comment : ''Merge pull request #262 from npasserini/multi-remotes\n\nMulti remotes'',
		#parentIds : [
			''0ff1bd036b0ec596efd4c667579041452f0f549d'',
			''52c405d2e3c300ed2114e810ed300e6423c58aa2''
		]
	},
	HiExampleCommit {
		#id : ''e40bdd03b0367c75f7c8a45803cff6c26a860b63'',
		#comment : ''in IceHistoryBrowser, mark with bold current branch '',
		#parentIds : [
			''52c405d2e3c300ed2114e810ed300e6423c58aa2''
		]
	},
	HiExampleCommit {
		#id : ''bb8bdd0cd14b3ea4d625139fee3133b0cbd48690'',
		#comment : ''Fix for https://github.com/npasserini/iceberg/issues/263 (Epicea now needs a method #isEpiceaCache on repositories)'',
		#parentIds : [
			''ec039975ff49bebd395de6bb06f964c1cadfc285''
		]
	},
	HiExampleCommit {
		#id : ''52c405d2e3c300ed2114e810ed300e6423c58aa2'',
		#comment : ''add method needed for Epicea (ugly, but needed)'',
		#parentIds : [
			''4a4fff7daef5c00b28b73e4c56328a1a994dbfb4''
		]
	},
	HiExampleCommit {
		#id : ''4a4fff7daef5c00b28b73e4c56328a1a994dbfb4'',
		#comment : ''restore test of all remote backends'',
		#parentIds : [
			''a8781a2d1129c4eb7664866d24a622b19436690c''
		]
	},
	HiExampleCommit {
		#id : ''a8781a2d1129c4eb7664866d24a622b19436690c'',
		#comment : ''prepare  #pushTo: to just set the upstream if current upstream is nil'',
		#parentIds : [
			''ce1d18c7e58ae8f78eb4589b84f5eab15052f350''
		]
	},
	HiExampleCommit {
		#id : ''ce1d18c7e58ae8f78eb4589b84f5eab15052f350'',
		#comment : ''add IceMCGitRepository>>fetchFrom:'',
		#parentIds : [
			''2a8177ccb97443b7ecfd6263d8a73e146055f6a5''
		]
	},
	HiExampleCommit {
		#id : ''2a8177ccb97443b7ecfd6263d8a73e146055f6a5'',
		#comment : ''Handle detached head state.'',
		#parentIds : [
			''ab80abcabfa445312372ebdade3e79066eb3c216''
		]
	},
	HiExampleCommit {
		#id : ''ab80abcabfa445312372ebdade3e79066eb3c216'',
		#comment : ''Use branch name instead of branch, command expects a String.'',
		#parentIds : [
			''517067399726c8bd7c49af9b2726d17abad03ab2''
		]
	},
	HiExampleCommit {
		#id : ''517067399726c8bd7c49af9b2726d17abad03ab2'',
		#comment : ''Implement pullFrom: in IceMCRepository.'',
		#parentIds : [
			''982f3c79e1a91dda313412b8b5d322d650b75448''
		]
	},
	HiExampleCommit {
		#id : ''982f3c79e1a91dda313412b8b5d322d650b75448'',
		#comment : ''Fix bug in upstream computation, branch is not a string any more.'',
		#parentIds : [
			''924c56ecce0791425ad5a595447b2fb396b07e17''
		]
	},
	HiExampleCommit {
		#id : ''924c56ecce0791425ad5a595447b2fb396b07e17'',
		#comment : ''enhance a bit the cleaning of repositories after test'',
		#parentIds : [
			''7bdc1d0eac91c86c796c5902e3a1509bc9538ab9''
		]
	},
	HiExampleCommit {
		#id : ''7bdc1d0eac91c86c796c5902e3a1509bc9538ab9'',
		#comment : ''force a GC to ensure models (who are stored in weak dictionaries) are cleaned before runing the test. '',
		#parentIds : [
			''e759658c2a27d45ad37d0360cba4190fba2b608b''
		]
	},
	HiExampleCommit {
		#id : ''e759658c2a27d45ad37d0360cba4190fba2b608b'',
		#comment : ''added IceMCGitRepository>>lookupBranchNamed:inRemote: to git command line backend.\rTEMPORARY change of backend to test (since I\''m testing this ones) '',
		#parentIds : [
			''9fb0578cca9c37601ddbceac2e7f0e9671b9cb0a''
		]
	},
	HiExampleCommit {
		#id : ''9fb0578cca9c37601ddbceac2e7f0e9671b9cb0a'',
		#comment : ''IceRepository shareRepositoriesBetweenImages status before running tests.'',
		#parentIds : [
			''5c0fc4fe392ef8ca26f8bdc9a087b79e1e7574fb''
		]
	},
	HiExampleCommit {
		#id : ''5c0fc4fe392ef8ca26f8bdc9a087b79e1e7574fb'',
		#comment : ''Merging with 49b87936f5f00e4f8e9f3e41020e3d4affa1bcec'',
		#parentIds : [
			''927683045b6ba5b9c5fbc73ecb5f5ded2b0dc986'',
			''49b87936f5f00e4f8e9f3e41020e3d4affa1bcec''
		]
	},
	HiExampleCommit {
		#id : ''927683045b6ba5b9c5fbc73ecb5f5ded2b0dc986'',
		#comment : ''Fix several test issues.\r- clean IceRepositoryModel caches on test #setUp.\r- scenarioOutdatedLocalRepository was creating wrong repository, pointing to local origin and without branch name.\r- remove unnecesary #respondsTo: guard in testCreateRepoOnTopOfMonticelloLoadedCode, scenario will always exist.'',
		#parentIds : [
			''09f080714b5371592ab83fc9b476e0d0785c4ac0''
		]
	},
	HiExampleCommit {
		#id : ''49b87936f5f00e4f8e9f3e41020e3d4affa1bcec'',
		#comment : ''fix pushTo: for IceMCGitRepository'',
		#parentIds : [
			''09f080714b5371592ab83fc9b476e0d0785c4ac0''
		]
	},
	HiExampleCommit {
		#id : ''09f080714b5371592ab83fc9b476e0d0785c4ac0'',
		#comment : ''Fix testCloneTwiceSameOrigin'',
		#parentIds : [
			''83833ef38ec8a92a3c82457a23c671b98beceb55''
		]
	},
	HiExampleCommit {
		#id : ''83833ef38ec8a92a3c82457a23c671b98beceb55'',
		#comment : ''Fix #251.'',
		#parentIds : [
			''294362df93f95707c8b554083ea47f3ede26ef2b''
		]
	},
	HiExampleCommit {
		#id : ''294362df93f95707c8b554083ea47f3ede26ef2b'',
		#comment : ''fixing some tests (still more remaining)'',
		#parentIds : [
			''85cad2a0187b77f42a674d031a0445b53ca0f6f0''
		]
	},
	HiExampleCommit {
		#id : ''85cad2a0187b77f42a674d031a0445b53ca0f6f0'',
		#comment : ''make ModelCache a class instance variable (to prevent errors when duplicated names)\radd a compaction algoritm for caches (when they are too much, we can just reduce the memory footprint)'',
		#parentIds : [
			''ca1790a37de75e6ed204737697e880d4339bd6d5''
		]
	},
	HiExampleCommit {
		#id : ''ca1790a37de75e6ed204737697e880d4339bd6d5'',
		#comment : ''New way of computing differences between versions, fixes performance problems when importing big projects.'',
		#parentIds : [
			''29f005b056406b96aa48f9cfb364d3291673cd9a''
		]
	},
	HiExampleCommit {
		#id : ''29f005b056406b96aa48f9cfb364d3291673cd9a'',
		#comment : ''Get GitFileTree back\n'',
		#parentIds : [
			''8a310fa976a99ab0225e251257625e13738340c9''
		]
	},
	HiExampleCommit {
		#id : ''8a310fa976a99ab0225e251257625e13738340c9'',
		#comment : ''Merge with refs/remotes/origin/multi-remotes'',
		#parentIds : [
			''f972f5f4623e526d83ba2c545cbd7f877c79e364'',
			''ddbbee8b67217cb3a002ca030b57e9dca2fb2c24''
		]
	},
	HiExampleCommit {
		#id : ''f972f5f4623e526d83ba2c545cbd7f877c79e364'',
		#comment : ''Add a second changes tree to the HistoryBrowser, showing the difference between a commit and his (first) parent.'',
		#parentIds : [
			''bc7f6d2ec132c334c2c213df049248a3902eacd3''
		]
	},
	HiExampleCommit {
		#id : ''ddbbee8b67217cb3a002ca030b57e9dca2fb2c24'',
		#comment : ''added pushing to/pulling from info in repository detailed info'',
		#parentIds : [
			''bc7f6d2ec132c334c2c213df049248a3902eacd3''
		]
	},
	HiExampleCommit {
		#id : ''bc7f6d2ec132c334c2c213df049248a3902eacd3'',
		#comment : ''revert coloring of remotes for pull/push because is confusing.'',
		#parentIds : [
			''7279721ac19e6699cefaf77d8d6c0238affb6053''
		]
	},
	HiExampleCommit {
		#id : ''7279721ac19e6699cefaf77d8d6c0238affb6053'',
		#comment : ''- add colors/bold for remotes and branches (experimental)\r- some minor refactors'',
		#parentIds : [
			''da5a1908b13029e8832bd5f8e2cdae0761d7404b''
		]
	},
	HiExampleCommit {
		#id : ''da5a1908b13029e8832bd5f8e2cdae0761d7404b'',
		#comment : ''add create branch and switch to branch options to branchs tab'',
		#parentIds : [
			''46db3af58800d98eb6ba0e008f6ab0b286ee4d77''
		]
	},
	HiExampleCommit {
		#id : ''46db3af58800d98eb6ba0e008f6ab0b286ee4d77'',
		#comment : ''- IceRepositoriesBrowser: add #statusForBranch: in branchs tag \r- some recategorisations and refactors'',
		#parentIds : [
			''28fe998e1cd4f72ec56461161105d93368795e26''
		]
	},
	HiExampleCommit {
		#id : ''28fe998e1cd4f72ec56461161105d93368795e26'',
		#comment : ''Ensure that a branch exists before creating an IceBranch object. Handle cases in which remote branches do not exist for computing incoming/outgoing commits.\r'',
		#parentIds : [
			''3783180fab1f92f419c8bc38efb8eae2bc3d9729''
		]
	},
	HiExampleCommit {
		#id : ''3783180fab1f92f419c8bc38efb8eae2bc3d9729'',
		#comment : ''Fix test scenario #scenarioOutdatedLocalRepository\rIt was downloading the wrong branch.'',
		#parentIds : [
			''56ed733ffbf93ff2525acdf5eb6affb2699a29b4''
		]
	},
	HiExampleCommit {
		#id : ''56ed733ffbf93ff2525acdf5eb6affb2699a29b4'',
		#comment : ''add fetch option on remotes panel'',
		#parentIds : [
			''8baaf600084d96ef00f94503e2fa6150a2c30bc7''
		]
	},
	HiExampleCommit {
		#id : ''8baaf600084d96ef00f94503e2fa6150a2c30bc7'',
		#comment : ''add pull/push options to remotes list'',
		#parentIds : [
			''feec1c78dfc3fdc559139341f7585d221647329b''
		]
	},
	HiExampleCommit {
		#id : ''feec1c78dfc3fdc559139341f7585d221647329b'',
		#comment : ''add comment'',
		#parentIds : [
			''42a036cab18cd6a00648836c0cb8a21bed3bc4fa''
		]
	},
	HiExampleCommit {
		#id : ''42a036cab18cd6a00648836c0cb8a21bed3bc4fa'',
		#comment : ''Merge with refs/remotes/origin/multi-remotes'',
		#parentIds : [
			''d24a93be665b1ac0b460ddb86aa82e2f49b159ea'',
			''954e24053394c8be91bd9741b9600b3d2fb3733b''
		]
	},
	HiExampleCommit {
		#id : ''d24a93be665b1ac0b460ddb86aa82e2f49b159ea'',
		#comment : ''- hopefully fix the IceDiff error \r- refactor Iceberg-UI package (added some tags to better organise)'',
		#parentIds : [
			''3b366a4358d15540749f47893b549a80fa8d1149''
		]
	},
	HiExampleCommit {
		#id : ''3b366a4358d15540749f47893b549a80fa8d1149'',
		#comment : ''reverting since it was not the update, the problem'',
		#parentIds : [
			''13690d66f1acaf97992ac95c480f3d193bab2abd''
		]
	},
	HiExampleCommit {
		#id : ''13690d66f1acaf97992ac95c480f3d193bab2abd'',
		#comment : ''- another attempt of fix error when commiting\r- push window shows also commit details'',
		#parentIds : [
			''3f7eca38114f5ca3197fdae7dac2fd8aefd5916f''
		]
	},
	HiExampleCommit {
		#id : ''3f7eca38114f5ca3197fdae7dac2fd8aefd5916f'',
		#comment : ''- highlight of packages also should not take into account incoming/outgoing.\r- try to fix an error when updating a pane'',
		#parentIds : [
			''2dfdd9975055c0bb55881bf27e43e980750d4a66''
		]
	},
	HiExampleCommit {
		#id : ''2dfdd9975055c0bb55881bf27e43e980750d4a66'',
		#comment : ''ignoring package status on IceRepositoriesBrowser (while you may want to query his history, doing this as default is far too consuming)'',
		#parentIds : [
			''954e24053394c8be91bd9741b9600b3d2fb3733b''
		]
	},
	HiExampleCommit {
		#id : ''954e24053394c8be91bd9741b9600b3d2fb3733b'',
		#comment : ''adding incomingCommitsFrom:/outgoingCommitsTo: to IceRepository'',
		#parentIds : [
			''cb4c685215a1793f6cf7df7db0bba1519c067330''
		]
	},
	HiExampleCommit {
		#id : ''cb4c685215a1793f6cf7df7db0bba1519c067330'',
		#comment : ''added incomingCommitsFrom:/outgoingCommitsTo: logic'',
		#parentIds : [
			''4b749b3a353b932c47dd27c9f2692fe56654cefb''
		]
	},
	HiExampleCommit {
		#id : ''4b749b3a353b932c47dd27c9f2692fe56654cefb'',
		#comment : ''remove another leak'',
		#parentIds : [
			''543bcf8cafbf650feb2d7fa5841c0300f5371679''
		]
	},
	HiExampleCommit {
		#id : ''543bcf8cafbf650feb2d7fa5841c0300f5371679'',
		#comment : ''just formatting'',
		#parentIds : [
			''71caf3654c468f325721108bcc973b3430e08b79''
		]
	},
	HiExampleCommit {
		#id : ''71caf3654c468f325721108bcc973b3430e08b79'',
		#comment : ''remove IceRepositoryWorkConfiguration (it was far too much)'',
		#parentIds : [
			''ffb819228e2970e9601cdeb142eca17514bd68ae''
		]
	},
	HiExampleCommit {
		#id : ''ffb819228e2970e9601cdeb142eca17514bd68ae'',
		#comment : ''fixing a number of leaks when subscribing for announcements'',
		#parentIds : [
			''9f412fa9855a37cf2878a4f3d3ee121b3303c82b''
		]
	},
	HiExampleCommit {
		#id : ''9f412fa9855a37cf2878a4f3d3ee121b3303c82b'',
		#comment : ''- fix some announcement weaks'',
		#parentIds : [
			''77cf50bb60a93398fc7c58aa1e8e65bd481d75c6''
		]
	},
	HiExampleCommit {
		#id : ''77cf50bb60a93398fc7c58aa1e8e65bd481d75c6'',
		#comment : ''temporary removing a suscription to see what happens'',
		#parentIds : [
			''16f2bb80be722d4c75602704534856be952da33d''
		]
	},
	HiExampleCommit {
		#id : ''16f2bb80be722d4c75602704534856be952da33d'',
		#comment : ''clean up'',
		#parentIds : [
			''4f03fe71b1bd3d6fde879abd3ae6e39f1f894fea''
		]
	},
	HiExampleCommit {
		#id : ''4f03fe71b1bd3d6fde879abd3ae6e39f1f894fea'',
		#comment : ''more on using pullRemote/pushRemote instead origin'',
		#parentIds : [
			''2f1a2a2838d87f9fa4cd7d3ea42d616e4da20d1f''
		]
	},
	HiExampleCommit {
		#id : ''2f1a2a2838d87f9fa4cd7d3ea42d616e4da20d1f'',
		#comment : ''fix: ensure removing a remote detaches also pulling/pushing remotes\rfix: push to default repo was bad on UI'',
		#parentIds : [
			''3f53266ec8dde8738d75c5cadfbdcaeb5251a280''
		]
	},
	HiExampleCommit {
		#id : ''3f53266ec8dde8738d75c5cadfbdcaeb5251a280'',
		#comment : ''- default pull/push now use user defined remotes\r- pull/push can be set in "remotes" panel.'',
		#parentIds : [
			''74f8f751c29abd24869f326cf840754e633486d1''
		]
	},
	HiExampleCommit {
		#id : ''74f8f751c29abd24869f326cf840754e633486d1'',
		#comment : ''add a "work configuration" object to keep separated "pushing" and "pulling" remotes (other stuff may come later)'',
		#parentIds : [
			''3accedd71855c7d5466bc6bb84bf08837698f9e0''
		]
	},
	HiExampleCommit {
		#id : ''3accedd71855c7d5466bc6bb84bf08837698f9e0'',
		#comment : ''- add remove of remote and tests for add/remove remotes\r- implement corrent add/remove of remotes on UI \r- recategorisation of some methods in IceRemote '',
		#parentIds : [
			''2575c50c31686e5fe4eb57c0dee765dcd51e0062''
		]
	},
	HiExampleCommit {
		#id : ''ec039975ff49bebd395de6bb06f964c1cadfc285'',
		#comment : ''modify #addFilesToIndex: and #contentOfFileNamed:commit: so that they can manage files outised the code directory\n'',
		#parentIds : [
			''bcb797d7a5abcfce953cc3d0ab7eb1ca4dff0667''
		]
	},
	HiExampleCommit {
		#id : ''2575c50c31686e5fe4eb57c0dee765dcd51e0062'',
		#comment : ''refactor on browsers (adding browser icons, initial extents) '',
		#parentIds : [
			''c91ddfb591ce23c47558f7a3052412868aa1660f''
		]
	},
	HiExampleCommit {
		#id : ''c91ddfb591ce23c47558f7a3052412868aa1660f'',
		#comment : ''first version that supports add remotes/pull from/push to'',
		#parentIds : [
			''97fe28ebbfcbf09f645c38d1a45de4fa08f8c741''
		]
	},
	HiExampleCommit {
		#id : ''97fe28ebbfcbf09f645c38d1a45de4fa08f8c741'',
		#comment : ''fix on test: ensure location is removed before a clone happens'',
		#parentIds : [
			''135564ad0a669819616f2b22930d04593df496e7''
		]
	},
	HiExampleCommit {
		#id : ''135564ad0a669819616f2b22930d04593df496e7'',
		#comment : ''fixed GUI test'',
		#parentIds : [
			''cc11cc2ee491229c1ffb1ecbef6e580a271a11bf''
		]
	},
	HiExampleCommit {
		#id : ''cc11cc2ee491229c1ffb1ecbef6e580a271a11bf'',
		#comment : ''Merge with 21d8410538738d4e9b74c9b9a17a24e6d0957472'',
		#parentIds : [
			''9fbefc07c5a19bca7ab55e34fc80fc9e3fbc4f5a'',
			''21d8410538738d4e9b74c9b9a17a24e6d0957472''
		]
	},
	HiExampleCommit {
		#id : ''9fbefc07c5a19bca7ab55e34fc80fc9e3fbc4f5a'',
		#comment : ''some default sizes for the browsers '',
		#parentIds : [
			''8480ddf65fbb1613d47b56ecc7c81dd6e6551df2''
		]
	},
	HiExampleCommit {
		#id : ''8480ddf65fbb1613d47b56ecc7c81dd6e6551df2'',
		#comment : ''fix merge into branch problem'',
		#parentIds : [
			''4150ac2c231bd066bfa8eb33289e34209d382a3c''
		]
	},
	HiExampleCommit {
		#id : ''4150ac2c231bd066bfa8eb33289e34209d382a3c'',
		#comment : ''Merge with 21d8410538738d4e9b74c9b9a17a24e6d0957472'',
		#parentIds : [
			''1499b52ca9d90bbe5ce6a82a414f4841cd463b9a'',
			''21d8410538738d4e9b74c9b9a17a24e6d0957472''
		]
	},
	HiExampleCommit {
		#id : ''1499b52ca9d90bbe5ce6a82a414f4841cd463b9a'',
		#comment : ''remove non-used IceRepository creation methods'',
		#parentIds : [
			''3f54411825efe2cfd7216f449ba61387b638b786''
		]
	},
	HiExampleCommit {
		#id : ''3f54411825efe2cfd7216f449ba61387b638b786'',
		#comment : ''fix testCloneAtAbsoluteLocation'',
		#parentIds : [
			''ca8c99256ede280b301f2c1bfdad3bef74da6f24''
		]
	},
	HiExampleCommit {
		#id : ''ca8c99256ede280b301f2c1bfdad3bef74da6f24'',
		#comment : ''fix some more tests'',
		#parentIds : [
			''9534feba9a6af52416e523a29c12142947c68dd0''
		]
	},
	HiExampleCommit {
		#id : ''9534feba9a6af52416e523a29c12142947c68dd0'',
		#comment : ''Merge with refs/remotes/origin/separate-creator-from-repository'',
		#parentIds : [
			''5202d8a98b7fbfd8311415ae8e160256cd6176e6'',
			''8d03335cd0d6b8af5c3a1783bd61a07cf0695c1a''
		]
	},
	HiExampleCommit {
		#id : ''5202d8a98b7fbfd8311415ae8e160256cd6176e6'',
		#comment : ''Do not use our current branch to find out the new branch when checking out. Fixes #testLoadCommit'',
		#parentIds : [
			''48e3555e4d21f670a1180b8a5e05dc0b53287945''
		]
	},
	HiExampleCommit {
		#id : ''8d03335cd0d6b8af5c3a1783bd61a07cf0695c1a'',
		#comment : ''fix another test'',
		#parentIds : [
			''48e3555e4d21f670a1180b8a5e05dc0b53287945''
		]
	},
	HiExampleCommit {
		#id : ''48e3555e4d21f670a1180b8a5e05dc0b53287945'',
		#comment : ''more on refactor and fix tests'',
		#parentIds : [
			''d5f792b99ca6c8b11dae77df588f73756ca225d5''
		]
	},
	HiExampleCommit {
		#id : ''d5f792b99ca6c8b11dae77df588f73756ca225d5'',
		#comment : ''fix some errors  with branches \rrefactor tests to take into account the existence of IceRepositoryCreator'',
		#parentIds : [
			''a6040a276d7ad3a755d9851a35c173b281c79f64''
		]
	},
	HiExampleCommit {
		#id : ''a6040a276d7ad3a755d9851a35c173b281c79f64'',
		#comment : ''- fix create repository when cloning from branch\r - use #isSameUrl: to find equivallent repos instead just url comparisson (because like that we can know that a https:// may be same url as git@... no idea if this is in the long way correct)\r'',
		#parentIds : [
			''40c6d7cb3a27a1935efb490ba23c4a8527ca2b85''
		]
	},
	HiExampleCommit {
		#id : ''bcb797d7a5abcfce953cc3d0ab7eb1ca4dff0667'',
		#comment : ''fix bug in #stagedPackageNames when IceRepository has a subdirectory.\rAdd a test for that but is not very good. Should be refactored to run all the test suite on bot a repo with a subdirectory and without.\n'',
		#parentIds : [
			''549403f66c808027f3d82ceced457d5e1f1a3667''
		]
	},
	HiExampleCommit {
		#id : ''40c6d7cb3a27a1935efb490ba23c4a8527ca2b85'',
		#comment : ''- fix a loop\r- add commit info panel to syncronizing browser'',
		#parentIds : [
			''540d1e16a7231d67802ac592581880c936c2cba9''
		]
	},
	HiExampleCommit {
		#id : ''540d1e16a7231d67802ac592581880c936c2cba9'',
		#comment : ''move branch variable to backend'',
		#parentIds : [
			''e3febab5fa9f7d0be36d783dadde34d22899210c''
		]
	},
	HiExampleCommit {
		#id : ''e3febab5fa9f7d0be36d783dadde34d22899210c'',
		#comment : ''specify branch name in clone operation (none for default) '',
		#parentIds : [
			''065ecda8c3705ae24a988af9c1f17bf51b6b4545''
		]
	},
	HiExampleCommit {
		#id : ''065ecda8c3705ae24a988af9c1f17bf51b6b4545'',
		#comment : ''more removals'',
		#parentIds : [
			''64d260f0357260acc492247d47d0b2a195831fc6''
		]
	},
	HiExampleCommit {
		#id : ''64d260f0357260acc492247d47d0b2a195831fc6'',
		#comment : ''- continue removing hopefully innecesary accessors\r- some refactors\r- history browser now show also a "commit info" panel with more extended data.'',
		#parentIds : [
			''3555c7ae48dfba29440435e81d9714cc4eb3f358''
		]
	},
	HiExampleCommit {
		#id : ''3555c7ae48dfba29440435e81d9714cc4eb3f358'',
		#comment : ''- fix repository creation on metacello adaptors.\r- remove some methods that should not be called.\r- put some halts in others that also should not be called, but I want to catch the users of it'',
		#parentIds : [
			''418b5b8fee0513437eb807b32ed68fafd87646f5''
		]
	},
	HiExampleCommit {
		#id : ''418b5b8fee0513437eb807b32ed68fafd87646f5'',
		#comment : ''some recategorisations'',
		#parentIds : [
			''e7070fdcceabd98c8a7899ac2b73e08438246faa''
		]
	},
	HiExampleCommit {
		#id : ''e7070fdcceabd98c8a7899ac2b73e08438246faa'',
		#comment : ''some recategorisations'',
		#parentIds : [
			''e30a9497753a031a131803349dfc0055880ef723''
		]
	},
	HiExampleCommit {
		#id : ''e30a9497753a031a131803349dfc0055880ef723'',
		#comment : ''correct location to use when cchecking for clone.'',
		#parentIds : [
			''aca91e63ff59f1dadb080708f648952d858c2323''
		]
	},
	HiExampleCommit {
		#id : ''aca91e63ff59f1dadb080708f648952d858c2323'',
		#comment : ''remove usage of #origin (WIP)\r\n'',
		#parentIds : [
			''00b84f453961467b2d403821bf7d7267a1d0da28''
		]
	},
	HiExampleCommit {
		#id : ''00b84f453961467b2d403821bf7d7267a1d0da28'',
		#comment : ''start remove unnecesary methods'',
		#parentIds : [
			''949b7cf94e0fee001685987d444222e40cb72b3c''
		]
	},
	HiExampleCommit {
		#id : ''549403f66c808027f3d82ceced457d5e1f1a3667'',
		#comment : ''fix merge d611f30e12f01657ee690b42ddb2722cb024e453 (merge interface does not allow to edit confilicting commits)\n'',
		#parentIds : [
			''d611f30e12f01657ee690b42ddb2722cb024e453''
		]
	},
	HiExampleCommit {
		#id : ''949b7cf94e0fee001685987d444222e40cb72b3c'',
		#comment : ''fist version, created IceRepositoryCreator and a couple of cases'',
		#parentIds : [
			''21d8410538738d4e9b74c9b9a17a24e6d0957472''
		]
	},
	HiExampleCommit {
		#id : ''d611f30e12f01657ee690b42ddb2722cb024e453'',
		#comment : ''Merging with 335678b3d1685b6cafecd3358ab04ade1f701a82\n'',
		#parentIds : [
			''09e6701f1fa09f8cd4fdc023ab06904459f1ad4a'',
			''335678b3d1685b6cafecd3358ab04ade1f701a82''
		]
	},
	HiExampleCommit {
		#id : ''09e6701f1fa09f8cd4fdc023ab06904459f1ad4a'',
		#comment : ''"Merge commit f1dcb3db8a16d0108d282ae6d521cfe9d3cbc38d"\n'',
		#parentIds : [
			''1458d81414487765ba3df5e42eceda2892d4f4a9'',
			''f1dcb3db8a16d0108d282ae6d521cfe9d3cbc38d''
		]
	},
	HiExampleCommit {
		#id : ''21d8410538738d4e9b74c9b9a17a24e6d0957472'',
		#comment : ''fix a problem when itializating IceRepository (subdirectory has to be set just once)'',
		#parentIds : [
			''30db073500fbe10435b122cbeaddc0688536af51''
		]
	},
	HiExampleCommit {
		#id : ''30db073500fbe10435b122cbeaddc0688536af51'',
		#comment : ''fix a problem on gitlocal resolve of directories\rchange again synchronize size\r'',
		#parentIds : [
			''bc1c4875f44f3a1e3fefad34563ab7a7a4b5cd15''
		]
	},
	HiExampleCommit {
		#id : ''335678b3d1685b6cafecd3358ab04ade1f701a82'',
		#comment : ''Saving files into the working copy does automatically add them to the index.\rFix repository subdirectory management when writing files to the working copy.\rFix preprocess to remove quotes from escaped strings (similar to bash behaviour).\rRemove references to gitfiletree gitCommand: method, we should use IceGitCommand instead.\r'',
		#parentIds : [
			''f1dcb3db8a16d0108d282ae6d521cfe9d3cbc38d''
		]
	},
	HiExampleCommit {
		#id : ''f1dcb3db8a16d0108d282ae6d521cfe9d3cbc38d'',
		#comment : ''Fix #stagedFiles'',
		#parentIds : [
			''6a615df41e76dfaf1744363dd5709eb6ea568546''
		]
	},
	HiExampleCommit {
		#id : ''bc1c4875f44f3a1e3fefad34563ab7a7a4b5cd15'',
		#comment : ''Change the width of the branches column'',
		#parentIds : [
			''f6d91a740e33815e2961f0a37fb3ab03eb761e4d''
		]
	},
	HiExampleCommit {
		#id : ''f6d91a740e33815e2961f0a37fb3ab03eb761e4d'',
		#comment : ''Merge pull request #234 from npasserini/fix-messages\n\nChange the title of the publish browser diff panel.'',
		#parentIds : [
			''07784ff113c66dbefb725221d8a1c94374cbba08'',
			''879f98413e00343f554c15d348277f05725ae553''
		]
	},
	HiExampleCommit {
		#id : ''879f98413e00343f554c15d348277f05725ae553'',
		#comment : ''Change the title of the publish browser diff panel.'',
		#parentIds : [
			''07784ff113c66dbefb725221d8a1c94374cbba08''
		]
	},
	HiExampleCommit {
		#id : ''07784ff113c66dbefb725221d8a1c94374cbba08'',
		#comment : ''Change publish for push for consistency.'',
		#parentIds : [
			''972853f4fdfdbd0399909d98687f59c377660837''
		]
	},
	HiExampleCommit {
		#id : ''1458d81414487765ba3df5e42eceda2892d4f4a9'',
		#comment : ''"refactor saveContent:inWorkingCopyFile: to have a common version for both backend and always use the repository root to handle relative pathes"\n'',
		#parentIds : [
			''a3c7b715721484ed0188534a241e48ce0c2e7af3''
		]
	},
	HiExampleCommit {
		#id : ''972853f4fdfdbd0399909d98687f59c377660837'',
		#comment : ''Fix #chooseTargetBranch has to set targetBranch variable.'',
		#parentIds : [
			''e0b4f1d26b7f6fa08db270fd2067464adf664419''
		]
	},
	HiExampleCommit {
		#id : ''e0b4f1d26b7f6fa08db270fd2067464adf664419'',
		#comment : ''Add merge into current branch actions.'',
		#parentIds : [
			''8bd4cdb2b456a9be69f650f31497e4c64235e0a8''
		]
	},
	HiExampleCommit {
		#id : ''8bd4cdb2b456a9be69f650f31497e4c64235e0a8'',
		#comment : ''New merge mechanism'',
		#parentIds : [
			''d302b0a42a6924fa3298fecbaeb81c24d5d91953''
		]
	},
	HiExampleCommit {
		#id : ''d302b0a42a6924fa3298fecbaeb81c24d5d91953'',
		#comment : ''Change default size of synchronize window.'',
		#parentIds : [
			''71582f6101e750d12f819d82ffb7d42ca8df22b4''
		]
	},
	HiExampleCommit {
		#id : ''71582f6101e750d12f819d82ffb7d42ca8df22b4'',
		#comment : ''fix MetacelloPharoCommonPlatform>>extractTypeFromDescription: to answer iceberg types as well.'',
		#parentIds : [
			''b1edaeb711195f73b0f899faea644b3e91aaa8be''
		]
	},
	HiExampleCommit {
		#id : ''b1edaeb711195f73b0f899faea644b3e91aaa8be'',
		#comment : ''Merge pull request #229 from npasserini/add-gitlocal\n\nadd "gitlocal" capability'',
		#parentIds : [
			''4409ab5e51c1d90640bc828ea465ae542bc08e7f'',
			''5546d065a19bc5f2b6c6d4fe97e2ec18ba97aea9''
		]
	},
	HiExampleCommit {
		#id : ''a3c7b715721484ed0188534a241e48ce0c2e7af3'',
		#comment : ''Merge pull request #1 from npasserini/merged-with-demarey\n\nMerge with iceberg core repo'',
		#parentIds : [
			''4aec4ca0af834cff45e7b5eb7d59c2fa8ccfbbad'',
			''6a615df41e76dfaf1744363dd5709eb6ea568546''
		]
	},
	HiExampleCommit {
		#id : ''5546d065a19bc5f2b6c6d4fe97e2ec18ba97aea9'',
		#comment : ''add "gitlocal" capability'',
		#parentIds : [
			''4409ab5e51c1d90640bc828ea465ae542bc08e7f''
		]
	},
	HiExampleCommit {
		#id : ''6a615df41e76dfaf1744363dd5709eb6ea568546'',
		#comment : ''Merging with 4aec4ca0af834cff45e7b5eb7d59c2fa8ccfbbad'',
		#parentIds : [
			''4409ab5e51c1d90640bc828ea465ae542bc08e7f'',
			''4aec4ca0af834cff45e7b5eb7d59c2fa8ccfbbad''
		]
	},
	HiExampleCommit {
		#id : ''4409ab5e51c1d90640bc828ea465ae542bc08e7f'',
		#comment : ''Update #lastStoredRun of tests.'',
		#parentIds : [
			''0f63fbf4bdfd6a582e4eaef015dcbfbe3c8814c3''
		]
	},
	HiExampleCommit {
		#id : ''0f63fbf4bdfd6a582e4eaef015dcbfbe3c8814c3'',
		#comment : ''Fix tests.'',
		#parentIds : [
			''6d86d9c0ffa911104405ef8ba3e2f306c2eda5b1''
		]
	},
	HiExampleCommit {
		#id : ''6d86d9c0ffa911104405ef8ba3e2f306c2eda5b1'',
		#comment : ''Merge with 22f780066c1901660ec66f36663cc8aa735ac511'',
		#parentIds : [
			''f568e28744d175f04122ac02cdc882489f9b968c'',
			''22f780066c1901660ec66f36663cc8aa735ac511''
		]
	},
	HiExampleCommit {
		#id : ''f568e28744d175f04122ac02cdc882489f9b968c'',
		#comment : ''Fix how to get branches. Iceberg backends have the responsibility of transforming their internal representation of a branch into IceBranches.'',
		#parentIds : [
			''449936235d30b69287666ea6b9765af701192209''
		]
	},
	HiExampleCommit {
		#id : ''22f780066c1901660ec66f36663cc8aa735ac511'',
		#comment : ''enhancements to settings'',
		#parentIds : [
			''449936235d30b69287666ea6b9765af701192209''
		]
	},
	HiExampleCommit {
		#id : ''4aec4ca0af834cff45e7b5eb7d59c2fa8ccfbbad'',
		#comment : ''fix argument type for libgit in #commitIndexWithMessage:andParents:\radded support to  get the list of staged files or packages.\rrefactor commitPackages* methods to factorize code and move iceberg commit notification to a commit method not specific to "utility" methods realted to packages.'',
		#parentIds : [
			''03898d6d5e26c1c47ccd886acc75ed0e860294a4''
		]
	},
	HiExampleCommit {
		#id : ''449936235d30b69287666ea6b9765af701192209'',
		#comment : ''Fix version writer: do not write metadata.'',
		#parentIds : [
			''dae1f35e9cae9cd764fb908f6476521c3b58bc86''
		]
	},
	HiExampleCommit {
		#id : ''03898d6d5e26c1c47ccd886acc75ed0e860294a4'',
		#comment : ''add #modifiedFiles and #modifiedPackages to IcebergRepository API.\rmove tests to IceRepository tests so that tests run on both backend.\rimplement #contentOfFileNamed:commit: and #saveContent:inWorkingCopyfile: for gitfiletree backend'',
		#parentIds : [
			''3c91107f46cebb1f89971ecd99017a80c34a5374''
		]
	},
	HiExampleCommit {
		#id : ''dae1f35e9cae9cd764fb908f6476521c3b58bc86'',
		#comment : ''Fixing bad merge of properties'',
		#parentIds : [
			''5c0c0ec199deae393a6226d99d2864e4c0d37f98''
		]
	},
	HiExampleCommit {
		#id : ''5c0c0ec199deae393a6226d99d2864e4c0d37f98'',
		#comment : ''Merge branch \''repackaging\'' into dev-0.4\n\nConflicts:\n\tBaselineOfIceberg.package/BaselineOfIceberg.class/instance/baseline..st\n\tBaselineOfIceberg.package/BaselineOfIceberg.class/methodProperties.json\n'',
		#parentIds : [
			''741387aca373c7004bc93ecb6cf2b2d128f98397'',
			''9fb8eb641a699a1b6737c463160fa048098462dd''
		]
	},
	HiExampleCommit {
		#id : ''9fb8eb641a699a1b6737c463160fa048098462dd'',
		#comment : ''Add changes to MetacelloPharoCommonPlatform as an independent package inside Iceberg.'',
		#parentIds : [
			''ea0efc633e523325313e9f544599ec1e24fd9317''
		]
	},
	HiExampleCommit {
		#id : ''741387aca373c7004bc93ecb6cf2b2d128f98397'',
		#comment : ''Remove method from class, it is provided by test.'',
		#parentIds : [
			''94e8ee6f52f21d11fa61fc8b4b75448d052cac25''
		]
	},
	HiExampleCommit {
		#id : ''94e8ee6f52f21d11fa61fc8b4b75448d052cac25'',
		#comment : ''Fix typo in package name.'',
		#parentIds : [
			''fc16e52d6c0f90ad8c542cecf0e11d8854d06b97''
		]
	},
	HiExampleCommit {
		#id : ''fc16e52d6c0f90ad8c542cecf0e11d8854d06b97'',
		#comment : ''Add packages for Tests to the baseline.'',
		#parentIds : [
			''17c85d146ca977b814e4b7929005f34c72a715bb''
		]
	},
	HiExampleCommit {
		#id : ''17c85d146ca977b814e4b7929005f34c72a715bb'',
		#comment : ''Fix #193 - Merge should not leave a dirty index. '',
		#parentIds : [
			''1b6af45ee4438c3b622387dee18c0e377e8ff8a9''
		]
	},
	HiExampleCommit {
		#id : ''1b6af45ee4438c3b622387dee18c0e377e8ff8a9'',
		#comment : ''Merge with 0ff1bd036b0ec596efd4c667579041452f0f549d'',
		#parentIds : [
			''5e468ff79418d6d0d040ce011d4ff82fae5e2cc2'',
			''0ff1bd036b0ec596efd4c667579041452f0f549d''
		]
	},
	HiExampleCommit {
		#id : ''5e468ff79418d6d0d040ce011d4ff82fae5e2cc2'',
		#comment : ''Add #versionsFor: to IceBranch'',
		#parentIds : [
			''91ba10bb5becb9a31a161f3fce419ad5cf53b25b''
		]
	},
	HiExampleCommit {
		#id : ''91ba10bb5becb9a31a161f3fce419ad5cf53b25b'',
		#comment : ''Use #setTargetId: because #setTarget: only is valid for references, and we have a commit.'',
		#parentIds : [
			''5a252c8991d2da09827ef99a38bbcae12c75edf7''
		]
	},
	HiExampleCommit {
		#id : ''5a252c8991d2da09827ef99a38bbcae12c75edf7'',
		#comment : ''Merge with 381a402f0eb139fe959b46698d3195eda3dc4760'',
		#parentIds : [
			''ca116286fe539b5e4faaae57bd4647cc8edd9a0a'',
			''381a402f0eb139fe959b46698d3195eda3dc4760''
		]
	},
	HiExampleCommit {
		#id : ''ca116286fe539b5e4faaae57bd4647cc8edd9a0a'',
		#comment : ''Fix index management on merge: always checkout the merge result because otherwise you get a dirty index after commit.'',
		#parentIds : [
			''d2b55a3e6d6f70fbec10ba6992a4d94adeb25db6''
		]
	},
	HiExampleCommit {
		#id : ''d2b55a3e6d6f70fbec10ba6992a4d94adeb25db6'',
		#comment : ''Remove halt.'',
		#parentIds : [
			''2a254753577145d990ec0555934b5ace0f91ca2d''
		]
	},
	HiExampleCommit {
		#id : ''381a402f0eb139fe959b46698d3195eda3dc4760'',
		#comment : ''Merge with 2a254753577145d990ec0555934b5ace0f91ca2d'',
		#parentIds : [
			''e11410a063cdf424161d00e39cfa0483c6700ae0'',
			''2a254753577145d990ec0555934b5ace0f91ca2d''
		]
	},
	HiExampleCommit {
		#id : ''2a254753577145d990ec0555934b5ace0f91ca2d'',
		#comment : ''Use just #basename of the branch for merging.'',
		#parentIds : [
			''167fe9091c114b0d5f6f4b76d072d85b2ada0a64''
		]
	},
	HiExampleCommit {
		#id : ''167fe9091c114b0d5f6f4b76d072d85b2ada0a64'',
		#comment : ''LGitCommitBuilder has to receive a ref-name and not a LGitReference object.'',
		#parentIds : [
			''6e9184b1f4d5b2d8853ba8c369a5610c44eb80f3''
		]
	},
	HiExampleCommit {
		#id : ''6e9184b1f4d5b2d8853ba8c369a5610c44eb80f3'',
		#comment : ''Adding #name to a commit. Missing it avoided to be used as a HEAD for a repository.'',
		#parentIds : [
			''0ffb383606a2de1ffbac7201b1ec273e2b64e640''
		]
	},
	HiExampleCommit {
		#id : ''0ffb383606a2de1ffbac7201b1ec273e2b64e640'',
		#comment : ''Add #basename to branches and remove #displayName (which is already covered by #name). Fixed implementation of branch collection methods in IceRepository.'',
		#parentIds : [
			''b38b5e7034e2c91cf9a3e77b0fc276c98712532c''
		]
	},
	HiExampleCommit {
		#id : ''b38b5e7034e2c91cf9a3e77b0fc276c98712532c'',
		#comment : ''Fixed #pull, should not leave index modified.'',
		#parentIds : [
			''ea0efc633e523325313e9f544599ec1e24fd9317''
		]
	},
	HiExampleCommit {
		#id : ''ea0efc633e523325313e9f544599ec1e24fd9317'',
		#comment : ''Fix branches (vs. branch names) management.\rImproved double dispatch commit walk.'',
		#parentIds : [
			''3a182bfe1b9651eafaf6bae8363ded8a4131f1dc''
		]
	},
	HiExampleCommit {
		#id : ''3a182bfe1b9651eafaf6bae8363ded8a4131f1dc'',
		#comment : ''Add missing packages.'',
		#parentIds : [
			''0dc463c7c8d11fdf3d64618f29abdc598e7b7e8c''
		]
	},
	HiExampleCommit {
		#id : ''0dc463c7c8d11fdf3d64618f29abdc598e7b7e8c'',
		#comment : ''Add different classes to handle local and remote branches.'',
		#parentIds : [
			''a5d0de7668e6bf679a31a4a8f4ad6042eca96474''
		]
	},
	HiExampleCommit {
		#id : ''3c91107f46cebb1f89971ecd99017a80c34a5374'',
		#comment : ''push addFilesToIndex: and commitIndexWithMessage:andParents: as Core API of IceRepository.\rrefactor to use mainly this new API.\rrename commit methods to have more intention revealing names\rremove not-used code in IceMCGitRepository: getVersions.. and gitVersions ...'',
		#parentIds : [
			''28e7b2e85ed5da2edb9d1cd19996681089d1cffe''
		]
	},
	HiExampleCommit {
		#id : ''a5d0de7668e6bf679a31a4a8f4ad6042eca96474'',
		#comment : ''Move #merge:into to TIceRepositoryBackend, so it is shared also for Libgit backend.'',
		#parentIds : [
			''c99ad45115b250d23194583d62b746beeaa5bae4''
		]
	},
	HiExampleCommit {
		#id : ''c99ad45115b250d23194583d62b746beeaa5bae4'',
		#comment : ''Introduce "unborn" branches, to handle enpty repositories. Fixes #127'',
		#parentIds : [
			''6ed390951fde7c629b97b693b9e915753f635f09''
		]
	},
	HiExampleCommit {
		#id : ''28e7b2e85ed5da2edb9d1cd19996681089d1cffe'',
		#comment : ''allow to write content to an iceberg repository working copy'',
		#parentIds : [
			''7301a323cfd78fe801ce0625f2126b41eeca7cc8''
		]
	},
	HiExampleCommit {
		#id : ''7301a323cfd78fe801ce0625f2126b41eeca7cc8'',
		#comment : ''Add support to read arbitrary file content from a git tree'',
		#parentIds : [
			''3ad9357056971491ecf5cc94de2257844f531e91''
		]
	},
	HiExampleCommit {
		#id : ''3ad9357056971491ecf5cc94de2257844f531e91'',
		#comment : ''rename commit:withMessage: and co with commitMcVersions:withMessage:'',
		#parentIds : [
			''8275ca4f75d96e491630cb87a948d2abd6596ebd''
		]
	},
	HiExampleCommit {
		#id : ''8275ca4f75d96e491630cb87a948d2abd6596ebd'',
		#comment : ''split commit in two parts: add files to index and then commit the index'',
		#parentIds : [
			''e11410a063cdf424161d00e39cfa0483c6700ae0''
		]
	},
	HiExampleCommit {
		#id : ''6ed390951fde7c629b97b693b9e915753f635f09'',
		#comment : ''Remove references to MCFileTreeGitRemoteRepository from IceAbstractTest'',
		#parentIds : [
			''e8514b3d02d686691b9d7455b48134ebc8231c84''
		]
	},
	HiExampleCommit {
		#id : ''e8514b3d02d686691b9d7455b48134ebc8231c84'',
		#comment : ''Implement our own metadata-less cypress writers.'',
		#parentIds : [
			''3cf37b04dc8edb89aba3437854e45b0c722a13f9''
		]
	},
	HiExampleCommit {
		#id : ''3cf37b04dc8edb89aba3437854e45b0c722a13f9'',
		#comment : ''Fix baseline #4'',
		#parentIds : [
			''485c2abe0355694311166b6132031897308548a8''
		]
	},
	HiExampleCommit {
		#id : ''485c2abe0355694311166b6132031897308548a8'',
		#comment : ''Fix baseline #3'',
		#parentIds : [
			''f6ccdcf4f47829053330804485cebac7390bdd6c''
		]
	},
	HiExampleCommit {
		#id : ''f6ccdcf4f47829053330804485cebac7390bdd6c'',
		#comment : ''Fix baseline'',
		#parentIds : [
			''6f169d150855979502bc0efa0cec95152f7b80c3''
		]
	},
	HiExampleCommit {
		#id : ''6f169d150855979502bc0efa0cec95152f7b80c3'',
		#comment : ''Fix baseline'',
		#parentIds : [
			''5ea97017ac7a6fc56a0b63c583584f73684140cf''
		]
	},
	HiExampleCommit {
		#id : ''0ff1bd036b0ec596efd4c667579041452f0f549d'',
		#comment : ''Add images for creating repository\n'',
		#parentIds : [
			''6b48c65a3fbe9c38243e04cd7cbf00006a9d8a3e''
		]
	},
	HiExampleCommit {
		#id : ''5ea97017ac7a6fc56a0b63c583584f73684140cf'',
		#comment : ''Add OSSubprocess/ProcessWrapper as requirement for Iceberg tests'',
		#parentIds : [
			''a5953ebf8bd97b24378fcb3b2d6986a750552c83''
		]
	},
	HiExampleCommit {
		#id : ''a5953ebf8bd97b24378fcb3b2d6986a750552c83'',
		#comment : ''Add Iceberg-GitCommand as requirement from Iceberg-Tests'',
		#parentIds : [
			''b82a9b91626b2e8d7fdd44194463f26792dc3b89''
		]
	},
	HiExampleCommit {
		#id : ''b82a9b91626b2e8d7fdd44194463f26792dc3b89'',
		#comment : ''Remove references to GitFileTree'',
		#parentIds : [
			''433ee4bf28e54a8d54aa092f84a92e9319663bbd''
		]
	},
	HiExampleCommit {
		#id : ''433ee4bf28e54a8d54aa092f84a92e9319663bbd'',
		#comment : ''Fix one more reference to MCFIleTreeGitRepository from Iceberg core'',
		#parentIds : [
			''3bb612aebb8da9e88f4360b6685aa8d30f943b88''
		]
	},
	HiExampleCommit {
		#id : ''3bb612aebb8da9e88f4360b6685aa8d30f943b88'',
		#comment : ''Remove references from Iceberg core and tests to MCFileTreeGitRepository'',
		#parentIds : [
			''d1c9e13fcf15733ae14d5f80e798efa9ba8b3eec''
		]
	},
	HiExampleCommit {
		#id : ''6b48c65a3fbe9c38243e04cd7cbf00006a9d8a3e'',
		#comment : ''More images to readme\n'',
		#parentIds : [
			''73e994aa78542aa19428859b8e7027a1aba4c97b''
		]
	},
	HiExampleCommit {
		#id : ''73e994aa78542aa19428859b8e7027a1aba4c97b'',
		#comment : ''More little improvements to readme\n'',
		#parentIds : [
			''f7bd9e88c5b253330f0a586449c18a0e0caa5ffe''
		]
	},
	HiExampleCommit {
		#id : ''f7bd9e88c5b253330f0a586449c18a0e0caa5ffe'',
		#comment : ''Add image\n'',
		#parentIds : [
			''acb1784c6167d5988688ed91e4e35a0cfd3691f9''
		]
	},
	HiExampleCommit {
		#id : ''d1c9e13fcf15733ae14d5f80e798efa9ba8b3eec'',
		#comment : ''Fix typo.'',
		#parentIds : [
			''8b9803e2533befa4ff9f9adf834a3335e6d5d13a''
		]
	},
	HiExampleCommit {
		#id : ''8b9803e2533befa4ff9f9adf834a3335e6d5d13a'',
		#comment : ''Remove gitfiletree dependencies from default metacello load.'',
		#parentIds : [
			''97f0e79dddc5495fbfdb333e52da2d180cc2e39f''
		]
	},
	HiExampleCommit {
		#id : ''97f0e79dddc5495fbfdb333e52da2d180cc2e39f'',
		#comment : ''Split tests, removing references to UI features. Repackage them into UI-Tests'',
		#parentIds : [
			''389d4442b306b447c8244fa9f2c435fe541f3377''
		]
	},
	HiExampleCommit {
		#id : ''389d4442b306b447c8244fa9f2c435fe541f3377'',
		#comment : ''Create new package for GUI tests, and update baseline to load it.'',
		#parentIds : [
			''c43438ad158df42f94745fc303b82124a5769e0f''
		]
	},
	HiExampleCommit {
		#id : ''acb1784c6167d5988688ed91e4e35a0cfd3691f9'',
		#comment : ''Improve tutorial\n'',
		#parentIds : [
			''53279c874c68426ae04c80cf6c3878b9f2831b30''
		]
	},
	HiExampleCommit {
		#id : ''53279c874c68426ae04c80cf6c3878b9f2831b30'',
		#comment : ''Improve tutorial\n'',
		#parentIds : [
			''6bc17e49a297d2810fe884b9557bd3646a30f5b1''
		]
	},
	HiExampleCommit {
		#id : ''6bc17e49a297d2810fe884b9557bd3646a30f5b1'',
		#comment : ''Add 5 minute tutorial\n'',
		#parentIds : [
			''73834208c0b4e51d3081330b7f8eab96a717e006''
		]
	},
	HiExampleCommit {
		#id : ''c43438ad158df42f94745fc303b82124a5769e0f'',
		#comment : ''Forget about searchExistingRepository, it only brings trouble.'',
		#parentIds : [
			''26ab99ee237ae03ce12d801613d83cb393777787''
		]
	},
	HiExampleCommit {
		#id : ''26ab99ee237ae03ce12d801613d83cb393777787'',
		#comment : ''IceMCVersionInfo must be part of Iceberg-Core.\rAvoid its dependencies with GitFileTree.'',
		#parentIds : [
			''3d88351e20b083a9e50c82ed8e1e1bc3df5094f3''
		]
	},
	HiExampleCommit {
		#id : ''3d88351e20b083a9e50c82ed8e1e1bc3df5094f3'',
		#comment : ''Attepmt #27'',
		#parentIds : [
			''f2bb3d2a0695d1016fdf9b34dbbaa705ee69d740''
		]
	},
	HiExampleCommit {
		#id : ''f2bb3d2a0695d1016fdf9b34dbbaa705ee69d740'',
		#comment : ''Another attempt.'',
		#parentIds : [
			''5a482927d245426dd78a9438e9ebf4677cf05659''
		]
	},
	HiExampleCommit {
		#id : ''5a482927d245426dd78a9438e9ebf4677cf05659'',
		#comment : ''Change implementation of default backend: have the backends install themselves on class load.'',
		#parentIds : [
			''4dd1c17e001396b9dc3568a020f3f67e4ed72f82''
		]
	},
	HiExampleCommit {
		#id : ''4dd1c17e001396b9dc3568a020f3f67e4ed72f82'',
		#comment : ''Better implementation of selected backend type: it is only a string, not a class name. This avoids the problem that arised because IceRepository settings are read before installing the selected backend class.'',
		#parentIds : [
			''f29e1dcd043dcf47cb478448a915585c1f93f577''
		]
	},
	HiExampleCommit {
		#id : ''e11410a063cdf424161d00e39cfa0483c6700ae0'',
		#comment : ''Merge pull request #222 from estebanlm/fix-choose-directory\n\nfix changeLocation'',
		#parentIds : [
			''f6bb72ba403561611d4915e0649db7962b79aa92'',
			''371f224e0c5d79280a41f57e1674d25d9693b904''
		]
	},
	HiExampleCommit {
		#id : ''f29e1dcd043dcf47cb478448a915585c1f93f577'',
		#comment : ''Default backend type is nil when installing Iceberg, because there is no backend available at the moment. So, when #defaultBackendType is requested, if it is nil, then try to infer which one to use.'',
		#parentIds : [
			''4d89bd8c5f295d1dd80b27eab43f7d3194368172''
		]
	},
	HiExampleCommit {
		#id : ''4d89bd8c5f295d1dd80b27eab43f7d3194368172'',
		#comment : ''Small fix: #defaultBackendType is a class method.'',
		#parentIds : [
			''c3a8b6b04ef6ad254063816481dd5ae7f6314f9b''
		]
	},
	HiExampleCommit {
		#id : ''c3a8b6b04ef6ad254063816481dd5ae7f6314f9b'',
		#comment : ''Special handling for when there is no backend available.'',
		#parentIds : [
			''f8819ccd0dbdeb06508db6418ba118de02cccfe4''
		]
	},
	HiExampleCommit {
		#id : ''f8819ccd0dbdeb06508db6418ba118de02cccfe4'',
		#comment : ''Avoid direct references to IceMCGitRepository from Iceberg-Core'',
		#parentIds : [
			''52db0f44ccf9cb1c76ca56faea5322d71c787386''
		]
	},
	HiExampleCommit {
		#id : ''371f224e0c5d79280a41f57e1674d25d9693b904'',
		#comment : ''fix changeLocation'',
		#parentIds : [
			''f6bb72ba403561611d4915e0649db7962b79aa92''
		]
	},
	HiExampleCommit {
		#id : ''52db0f44ccf9cb1c76ca56faea5322d71c787386'',
		#comment : ''Move adapter classes to their specific packages.'',
		#parentIds : [
			''bfc2802fd02a47c578559970d1f084ca440ee9b0''
		]
	},
	HiExampleCommit {
		#id : ''bfc2802fd02a47c578559970d1f084ca440ee9b0'',
		#comment : ''Still one fix on the Baseline.'',
		#parentIds : [
			''1f39a04f959e87777164d987d9786b4d9083064f''
		]
	},
	HiExampleCommit {
		#id : ''1f39a04f959e87777164d987d9786b4d9083064f'',
		#comment : ''New baseline, default loads only libgit and not gitfiletree backend.'',
		#parentIds : [
			''2d9d7b0d44e594d403e020d9911a72d92b3de7c8''
		]
	},
	HiExampleCommit {
		#id : ''2d9d7b0d44e594d403e020d9911a72d92b3de7c8'',
		#comment : ''Move some git-file-tree related classes to an independent package.'',
		#parentIds : [
			''42d5350109b525cd09f64937c18edb70e32e3c76''
		]
	},
	HiExampleCommit {
		#id : ''42d5350109b525cd09f64937c18edb70e32e3c76'',
		#comment : ''Merge branch \''dev-0.4\'' into repackaging\n'',
		#parentIds : [
			''b87aaad7efd56e9237f7ea177162b6d8c88513c3'',
			''f6bb72ba403561611d4915e0649db7962b79aa92''
		]
	},
	HiExampleCommit {
		#id : ''b87aaad7efd56e9237f7ea177162b6d8c88513c3'',
		#comment : ''Merge remote-tracking branch \''origin/master\'' into repackaging\n\nConflicts:\n\tBaselineOfIceberg.package/BaselineOfIceberg.class/instance/baseline..st\n'',
		#parentIds : [
			''0995f28432970428ca3803a03db304cd0c3f3720'',
			''73834208c0b4e51d3081330b7f8eab96a717e006''
		]
	},
	HiExampleCommit {
		#id : ''f6bb72ba403561611d4915e0649db7962b79aa92'',
		#comment : ''Make libgit backend respond correctly to allBranchNames.\rAdd a test, refactor all branch tests in a separated (parameterized) class.\rRenamed #localBranches in backends to #localBranchNames, as it returns just strings.\rRemove prefix \''remote\'' from remote branch names.'',
		#parentIds : [
			''2dc909a3070c8c0ea8a8528f8a8af456bad5ad17''
		]
	},
	HiExampleCommit {
		#id : ''2dc909a3070c8c0ea8a8528f8a8af456bad5ad17'',
		#comment : ''Fix #219  - Implement local branches for Libgit backend.'',
		#parentIds : [
			''18c6a0a6e6c9f9c850ab6a27f8f61772ab28db96''
		]
	},
	HiExampleCommit {
		#id : ''18c6a0a6e6c9f9c850ab6a27f8f61772ab28db96'',
		#comment : ''Improve tests organization. \r- Separate Libgit specific tests out of the hierarchy of repository tests to allow for parameterization and not have them run twice.\r- Move scenarios to super class, allow them to be used in several tests.'',
		#parentIds : [
			''e94f5c3552a16ba6c461462339a0c1522f3f57e4''
		]
	},
	HiExampleCommit {
		#id : ''e94f5c3552a16ba6c461462339a0c1522f3f57e4'',
		#comment : ''Fix broken tests.'',
		#parentIds : [
			''0995f28432970428ca3803a03db304cd0c3f3720''
		]
	},
	HiExampleCommit {
		#id : ''0995f28432970428ca3803a03db304cd0c3f3720'',
		#comment : ''Remove pathAsString method, replace it by the standard #pathString'',
		#parentIds : [
			''bd1230656a09974cc1eede43ef4a483eb51bfe68''
		]
	},
	HiExampleCommit {
		#id : ''bd1230656a09974cc1eede43ef4a483eb51bfe68'',
		#comment : ''Add a headless test group.'',
		#parentIds : [
			''c5d19a23f29d46f5d9f6e998d52437d82b4dccba''
		]
	},
	HiExampleCommit {
		#id : ''c5d19a23f29d46f5d9f6e998d52437d82b4dccba'',
		#comment : ''Create new group for loading a headless Iceberg.'',
		#parentIds : [
			''d7cd38c0088ebe2990603bb8b9da4a745a4d17ff''
		]
	},
	HiExampleCommit {
		#id : ''d7cd38c0088ebe2990603bb8b9da4a745a4d17ff'',
		#comment : ''Rename IceCommitModelNew to IceCommitModel'',
		#parentIds : [
			''c1b2f570c563ebb4dde5ac5593fdfe2e6fe40692''
		]
	},
	HiExampleCommit {
		#id : ''c1b2f570c563ebb4dde5ac5593fdfe2e6fe40692'',
		#comment : ''Handle the case in which a version does not exist in an (older) commit in the repository.'',
		#parentIds : [
			''7ca1d520dcf5ad57fb65d7135c9aff127171432e''
		]
	},
	HiExampleCommit {
		#id : ''7ca1d520dcf5ad57fb65d7135c9aff127171432e'',
		#comment : ''Promoted Iceberg-UI category as a new package.'',
		#parentIds : [
			''1a799d3bd5408662c0e9565d2de547c50e4a168f''
		]
	},
	HiExampleCommit {
		#id : ''1a799d3bd5408662c0e9565d2de547c50e4a168f'',
		#comment : ''Fix add package.'',
		#parentIds : [
			''9313851dbca2b3a45fe36aa53644623c5331a8bc''
		]
	},
	HiExampleCommit {
		#id : ''9313851dbca2b3a45fe36aa53644623c5331a8bc'',
		#comment : ''Renamed class tag Iceberg-GUI to Iceberg-UI'',
		#parentIds : [
			''990d246c92796bee3ad084995c97be0ab439eb3b''
		]
	},
	HiExampleCommit {
		#id : ''990d246c92796bee3ad084995c97be0ab439eb3b'',
		#comment : ''Merge with b2c4c2faeaa80e9773fabd663806ff6acdd31731'',
		#parentIds : [
			''01cb1b441bf066475d0995c4b8963c3b65754494'',
			''b2c4c2faeaa80e9773fabd663806ff6acdd31731''
		]
	},
	HiExampleCommit {
		#id : ''01cb1b441bf066475d0995c4b8963c3b65754494'',
		#comment : ''Clean up IceDiff.'',
		#parentIds : [
			''377cf204966a373a5e0b6db077925cc363cbb7b0''
		]
	},
	HiExampleCommit {
		#id : ''b2c4c2faeaa80e9773fabd663806ff6acdd31731'',
		#comment : ''Ask the diff about what packages to commit, we can not trust in the #modified attribute of a working copy. \rRight now the diff returns all packages with changes, later this should be only the default and we should allow for a manual selection.'',
		#parentIds : [
			''f7881a98cad4dd46699ca619182c3bde1db49a4f''
		]
	},
	HiExampleCommit {
		#id : ''f7881a98cad4dd46699ca619182c3bde1db49a4f'',
		#comment : ''Fix bug in computin versions for a package.'',
		#parentIds : [
			''af3ccf56a3078f4891ca017c041a7a359b6371c3''
		]
	},
	HiExampleCommit {
		#id : ''af3ccf56a3078f4891ca017c041a7a359b6371c3'',
		#comment : ''Fixes #217.'',
		#parentIds : [
			''04b2f909686988f5b13a2c497dc3759eac6a91fa''
		]
	},
	HiExampleCommit {
		#id : ''04b2f909686988f5b13a2c497dc3759eac6a91fa'',
		#comment : ''Improved how to show a tag promotion to package in changes tree (see #83).'',
		#parentIds : [
			''377cf204966a373a5e0b6db077925cc363cbb7b0''
		]
	},
	HiExampleCommit {
		#id : ''377cf204966a373a5e0b6db077925cc363cbb7b0'',
		#comment : ''Small refactor on saved package management.'',
		#parentIds : [
			''3c301a311c8f4d58b073c0028951f11d1f6a1d9a''
		]
	},
	HiExampleCommit {
		#id : ''3c301a311c8f4d58b073c0028951f11d1f6a1d9a'',
		#comment : ''Working on #218 - Do not use savePackage: from tests.\r'',
		#parentIds : [
			''a1260f1a5955a5bfbb13c98bf089f54c0b76d4ff''
		]
	},
	HiExampleCommit {
		#id : ''a1260f1a5955a5bfbb13c98bf089f54c0b76d4ff'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''644c427471e3516f3354875cb08d9011881de825'',
			''d9bce7fc8ab2ff69fb5dc1041c508c9927d5c8d3''
		]
	},
	HiExampleCommit {
		#id : ''d9bce7fc8ab2ff69fb5dc1041c508c9927d5c8d3'',
		#comment : ''Remove old UI package.'',
		#parentIds : [
			''1fa5f1e755799cc3ca3309e7b236610b6e5246a2''
		]
	},
	HiExampleCommit {
		#id : ''644c427471e3516f3354875cb08d9011881de825'',
		#comment : ''Small fixes.'',
		#parentIds : [
			''1fa5f1e755799cc3ca3309e7b236610b6e5246a2''
		]
	},
	HiExampleCommit {
		#id : ''1fa5f1e755799cc3ca3309e7b236610b6e5246a2'',
		#comment : ''More improvements in repositories browser (#216)'',
		#parentIds : [
			''e7b9b02b71b99f2c0c3581c6d9efb3df6747f971''
		]
	},
	HiExampleCommit {
		#id : ''e7b9b02b71b99f2c0c3581c6d9efb3df6747f971'',
		#comment : ''Merge with 933482723a10e98e839f0c9791fc5cbef41e31f0'',
		#parentIds : [
			''21b3b8e3a58361e943f559daaa861b0ea275f4d9'',
			''933482723a10e98e839f0c9791fc5cbef41e31f0''
		]
	},
	HiExampleCommit {
		#id : ''21b3b8e3a58361e943f559daaa861b0ea275f4d9'',
		#comment : ''Some improvements for #216'',
		#parentIds : [
			''e439723bd2c724c076a78446fd6b33dee179f6bb''
		]
	},
	HiExampleCommit {
		#id : ''933482723a10e98e839f0c9791fc5cbef41e31f0'',
		#comment : ''More changes related to #211'',
		#parentIds : [
			''15cf59e18f8aec4e890a023e4d2c02e7b0bcf45a''
		]
	},
	HiExampleCommit {
		#id : ''15cf59e18f8aec4e890a023e4d2c02e7b0bcf45a'',
		#comment : ''Fixes misspelling #211'',
		#parentIds : [
			''20af86f5737f4ddd48ac1c2bab1fe3d910e7e7a3''
		]
	},
	HiExampleCommit {
		#id : ''20af86f5737f4ddd48ac1c2bab1fe3d910e7e7a3'',
		#comment : ''Fixes #215. We were missing to save the index object to the index file in .git/index.'',
		#parentIds : [
			''52d08b41bbf365d0c975e16426081610d3190b0d''
		]
	},
	HiExampleCommit {
		#id : ''52d08b41bbf365d0c975e16426081610d3190b0d'',
		#comment : ''Prueba.'',
		#parentIds : [
			''712bda6484de06cc0b01f22a8d90299933072c78''
		]
	},
	HiExampleCommit {
		#id : ''712bda6484de06cc0b01f22a8d90299933072c78'',
		#comment : ''Fix.'',
		#parentIds : [
			''7593011c1e6a93c49d9f32c0c2391670c935b88c''
		]
	},
	HiExampleCommit {
		#id : ''7593011c1e6a93c49d9f32c0c2391670c935b88c'',
		#comment : ''safasdf'',
		#parentIds : [
			''8ae168a61190cd7721b8b04a5efe9e51f84c8109''
		]
	},
	HiExampleCommit {
		#id : ''8ae168a61190cd7721b8b04a5efe9e51f84c8109'',
		#comment : ''Test commit '',
		#parentIds : [
			''f021eac94b1a399443bd333cd1a360bd26353239''
		]
	},
	HiExampleCommit {
		#id : ''f021eac94b1a399443bd333cd1a360bd26353239'',
		#comment : ''Fixes #214.'',
		#parentIds : [
			''e439723bd2c724c076a78446fd6b33dee179f6bb''
		]
	},
	HiExampleCommit {
		#id : ''e439723bd2c724c076a78446fd6b33dee179f6bb'',
		#comment : ''Callback changed and seems to receive the delta directly instead of a void* (=handle).'',
		#parentIds : [
			''9b5ef25d29e62fee7ab440a09e5643b81930bcbe''
		]
	},
	HiExampleCommit {
		#id : ''9b5ef25d29e62fee7ab440a09e5643b81930bcbe'',
		#comment : ''Delegate credentials creation.'',
		#parentIds : [
			''4bb28f460dcee2da80c3ffa744eecbd44e0961d7''
		]
	},
	HiExampleCommit {
		#id : ''4bb28f460dcee2da80c3ffa744eecbd44e0961d7'',
		#comment : ''Improving the handling of plaintext credentials.'',
		#parentIds : [
			''f766cfebdc0c7d0f14197fc029b770b665fc399b''
		]
	},
	HiExampleCommit {
		#id : ''f766cfebdc0c7d0f14197fc029b770b665fc399b'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''3c7dad3b9653d17126402bf5eda02ed5a6f5c270'',
			''afbc80cec271bd24c5e1f6863ee6068ee586d3e0''
		]
	},
	HiExampleCommit {
		#id : ''3c7dad3b9653d17126402bf5eda02ed5a6f5c270'',
		#comment : ''Partial changes to save credentials.\n'',
		#parentIds : [
			''afbc80cec271bd24c5e1f6863ee6068ee586d3e0''
		]
	},
	HiExampleCommit {
		#id : ''73834208c0b4e51d3081330b7f8eab96a717e006'',
		#comment : ''Change baseline to point to the new libgit2 repository.\n'',
		#parentIds : [
			''d0b17da50f08324114e48bbfc67bff3ccb44e12c''
		]
	},
	HiExampleCommit {
		#id : ''d0b17da50f08324114e48bbfc67bff3ccb44e12c'',
		#comment : ''Fix dependency to libgit tests.\nConflicts:\n\tBaselineOfIceberg.package/BaselineOfIceberg.class/instance/baseline..st\n'',
		#parentIds : [
			''6e66ae86d5f3fe0319208184a044f668ea4c6fb6''
		]
	},
	HiExampleCommit {
		#id : ''afbc80cec271bd24c5e1f6863ee6068ee586d3e0'',
		#comment : ''And another one.'',
		#parentIds : [
			''084370fab55792951e985ac243baf700b0eeb641''
		]
	},
	HiExampleCommit {
		#id : ''084370fab55792951e985ac243baf700b0eeb641'',
		#comment : ''Fix yet another typo.'',
		#parentIds : [
			''2aed773a427ee8849c96baa7fe7029bb188e682a''
		]
	},
	HiExampleCommit {
		#id : ''2aed773a427ee8849c96baa7fe7029bb188e682a'',
		#comment : ''Fix typo.'',
		#parentIds : [
			''e08ced301152fc384512ac2f2b52d3bb33af3ce4''
		]
	},
	HiExampleCommit {
		#id : ''e08ced301152fc384512ac2f2b52d3bb33af3ce4'',
		#comment : ''Fix dependency to libgit tests.'',
		#parentIds : [
			''32b4310272ddb6d2b553359817cf7defe357a193''
		]
	},
	HiExampleCommit {
		#id : ''32b4310272ddb6d2b553359817cf7defe357a193'',
		#comment : ''Libgit repository and branches renamed.'',
		#parentIds : [
			''90b77bd0d96062564bfb0b74452afcb965c40a35''
		]
	},
	HiExampleCommit {
		#id : ''90b77bd0d96062564bfb0b74452afcb965c40a35'',
		#comment : ''Make Metacello integration be tested using both scp and https remotes.'',
		#parentIds : [
			''fff9eb5cf8e296bd7a0333d5414d4a6c6ee15f7a''
		]
	},
	HiExampleCommit {
		#id : ''fff9eb5cf8e296bd7a0333d5414d4a6c6ee15f7a'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''17cce8e94e6ceb4247b03bf5f8aacedee6b15500'',
			''c7ab6f5da27573e4cc6846f52a223d7989b8bdbc''
		]
	},
	HiExampleCommit {
		#id : ''17cce8e94e6ceb4247b03bf5f8aacedee6b15500'',
		#comment : ''Reorganize .travis.yml\n'',
		#parentIds : [
			''c7ab6f5da27573e4cc6846f52a223d7989b8bdbc''
		]
	},
	HiExampleCommit {
		#id : ''c7ab6f5da27573e4cc6846f52a223d7989b8bdbc'',
		#comment : ''Add a basic implementation of parameterized test cases, which allows to run the same test case against all known Iceberg backends.'',
		#parentIds : [
			''4e18af22e87fb7cfda5f9d12993005e74ecdd4b8''
		]
	},
	HiExampleCommit {
		#id : ''4e18af22e87fb7cfda5f9d12993005e74ecdd4b8'',
		#comment : ''Add settings for selecting the default remote (https/scp).'',
		#parentIds : [
			''f9aacb820d6060ec1ff7dfef2ccba2f24ad0f055''
		]
	},
	HiExampleCommit {
		#id : ''f9aacb820d6060ec1ff7dfef2ccba2f24ad0f055'',
		#comment : ''Added an Https Remote to allow to handle such urls (see #210)'',
		#parentIds : [
			''11eb10c6e77400e50923048473e504bf448cf910''
		]
	},
	HiExampleCommit {
		#id : ''11eb10c6e77400e50923048473e504bf448cf910'',
		#comment : ''Remove #halt sends.'',
		#parentIds : [
			''e03a59bd4afcf67c7ee653d646a130089f1e5cc0''
		]
	},
	HiExampleCommit {
		#id : ''e03a59bd4afcf67c7ee653d646a130089f1e5cc0'',
		#comment : ''Fixes in the methods that obtained repository locations from the tests, to be able to work in Pharo 5.0.'',
		#parentIds : [
			''2e5f7c3215a6eb0ce1f893f1e56673fb1eb2cfe6''
		]
	},
	HiExampleCommit {
		#id : ''2e5f7c3215a6eb0ce1f893f1e56673fb1eb2cfe6'',
		#comment : ''Little hack to allow to see some parts of the history until I code a real solution for #208\r'',
		#parentIds : [
			''ebb867212c35f24eff6267997d2b93f797e7fee6''
		]
	},
	HiExampleCommit {
		#id : ''ebb867212c35f24eff6267997d2b93f797e7fee6'',
		#comment : ''Fix more accents\n'',
		#parentIds : [
			''38149715aa7ba487918e92a17d41660e98f55e4c''
		]
	},
	HiExampleCommit {
		#id : ''38149715aa7ba487918e92a17d41660e98f55e4c'',
		#comment : ''Fix accents\n'',
		#parentIds : [
			''b92087e7d42d22fced4dc1bcfdd34486487167c5''
		]
	},
	HiExampleCommit {
		#id : ''b92087e7d42d22fced4dc1bcfdd34486487167c5'',
		#comment : ''Clean up\n'',
		#parentIds : [
			''ba78e484f571bbecaf5f594efa53e11800880523''
		]
	},
	HiExampleCommit {
		#id : ''ba78e484f571bbecaf5f594efa53e11800880523'',
		#comment : ''Avoid direct usage of FileLocator localDirectory, for compatibility with Pharo 5'',
		#parentIds : [
			''9d0d068a884572ba56b891c6038fc8273625070d''
		]
	},
	HiExampleCommit {
		#id : ''9d0d068a884572ba56b891c6038fc8273625070d'',
		#comment : ''Fix how a test installs an IceTestCredentialProvider.'',
		#parentIds : [
			''cb63812c4938932e19cc31a0dfdd8cdfe6c10b4b''
		]
	},
	HiExampleCommit {
		#id : ''cb63812c4938932e19cc31a0dfdd8cdfe6c10b4b'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''56933873470f593ef6731b2e7e2542b266549c20'',
			''c70d817a68a3d30d139cd2b82ef47f5c750a36de''
		]
	},
	HiExampleCommit {
		#id : ''56933873470f593ef6731b2e7e2542b266549c20'',
		#comment : ''Fixes #207. Load stored Iceberg settings after loading Iceberg.\r'',
		#parentIds : [
			''c70d817a68a3d30d139cd2b82ef47f5c750a36de''
		]
	},
	HiExampleCommit {
		#id : ''c70d817a68a3d30d139cd2b82ef47f5c750a36de'',
		#comment : ''Merge with refs/remotes/origin/dev-0.4'',
		#parentIds : [
			''87aee1ed688c2f4ac96a2941ac0fbcfc9e1630ee'',
			''2ce5fe062979d00b3d7639071d44f9f96c152ba8''
		]
	},
	HiExampleCommit {
		#id : ''87aee1ed688c2f4ac96a2941ac0fbcfc9e1630ee'',
		#comment : ''Fixes #206 - Make libgit default backend'',
		#parentIds : [
			''2ce5fe062979d00b3d7639071d44f9f96c152ba8''
		]
	},
	HiExampleCommit {
		#id : ''2ce5fe062979d00b3d7639071d44f9f96c152ba8'',
		#comment : ''New dialog for asking for credentials (plaintext).\rImproved credentials provider management, creates a new instance for each login, to allow for counting tries.\r'',
		#parentIds : [
			''473cb92a48e79159af8be37bc48af21f492b73b9''
		]
	},
	HiExampleCommit {
		#id : ''473cb92a48e79159af8be37bc48af21f492b73b9'',
		#comment : ''Fix #203. Make IceMCGitRepository implement revparse: and revparseCommit:\n'',
		#parentIds : [
			''cb5c207c2fa339965a320a31df9fad0efe4b86f5''
		]
	},
	HiExampleCommit {
		#id : ''cb5c207c2fa339965a320a31df9fad0efe4b86f5'',
		#comment : ''Fix #203.\n'',
		#parentIds : [
			''6680a2b0de8b3908faecfdaea4b9bc2028e37402''
		]
	},
	HiExampleCommit {
		#id : ''6680a2b0de8b3908faecfdaea4b9bc2028e37402'',
		#comment : ''Fixes #202. Fixes #203\n'',
		#parentIds : [
			''d776ef104c6f286220b04ff96513abfe1581608f''
		]
	},
	HiExampleCommit {
		#id : ''d776ef104c6f286220b04ff96513abfe1581608f'',
		#comment : ''#201 Remove #repository accessor\n'',
		#parentIds : [
			''9de80a17cd35e9fadda45c7af266a870bce4d74b''
		]
	},
	HiExampleCommit {
		#id : ''9de80a17cd35e9fadda45c7af266a870bce4d74b'',
		#comment : ''#201 Rename repository variable to \''backend\''.\n'',
		#parentIds : [
			''372c0f39b6caed72a87e46ed9a285c0aaabfe3ac''
		]
	},
	HiExampleCommit {
		#id : ''372c0f39b6caed72a87e46ed9a285c0aaabfe3ac'',
		#comment : ''Fix branch name in baseline.\n'',
		#parentIds : [
			''b0efd93c1ca56dd0e31da78b33edaeac832c46c2''
		]
	},
	HiExampleCommit {
		#id : ''6e66ae86d5f3fe0319208184a044f668ea4c6fb6'',
		#comment : ''Fix related to https://github.com/npasserini/iceberg/issues/198\n'',
		#parentIds : [
			''6acc5ab52f1dcd539ba944ab51e83d72f32b2dc6''
		]
	},
	HiExampleCommit {
		#id : ''b0efd93c1ca56dd0e31da78b33edaeac832c46c2'',
		#comment : ''Change baseline, point to dev version in Libgit repository.\n'',
		#parentIds : [
			''a55c15f9d5916418f05fcd79102390d9d05a2971''
		]
	},
	HiExampleCommit {
		#id : ''a55c15f9d5916418f05fcd79102390d9d05a2971'',
		#comment : ''Fix #testImportOutdatedLocalRepository\rSkip #testCloneLowLevel\rAllow to install and uninstall credentials providers (during tests).\rImprove code organization in test setUp/tearDown\n'',
		#parentIds : [
			''75c6fac44dd0bc95795d1c69b755c1c714d74e3f''
		]
	},
	HiExampleCommit {
		#id : ''75c6fac44dd0bc95795d1c69b755c1c714d74e3f'',
		#comment : ''Use IceCredentialsProvider to give credentials to the push process.\n'',
		#parentIds : [
			''751151ad2d4a129dd4f53bc594ad338133618d56''
		]
	},
	HiExampleCommit {
		#id : ''751151ad2d4a129dd4f53bc594ad338133618d56'',
		#comment : ''Fix #testPull, use https instead of ssh.\n'',
		#parentIds : [
			''45dcc7cecd800a658d69656bd089bc867856a85b''
		]
	},
	HiExampleCommit {
		#id : ''45dcc7cecd800a658d69656bd089bc867856a85b'',
		#comment : ''Fixing tests.\n'',
		#parentIds : [
			''855608e2517c93c1444261fd827863e88e2b5e8d''
		]
	},
	HiExampleCommit {
		#id : ''855608e2517c93c1444261fd827863e88e2b5e8d'',
		#comment : ''"Merge commit 8e081d9012907acd7ec853b500eb1e9c1b3355fa"\n'',
		#parentIds : [
			''562c89c38c9a1a55b20e1feacad2369b4d3e3789'',
			''8e081d9012907acd7ec853b500eb1e9c1b3355fa''
		]
	},
	HiExampleCommit {
		#id : ''562c89c38c9a1a55b20e1feacad2369b4d3e3789'',
		#comment : ''Improved tag handling. You can now checkout a tag. FIxes #195\n'',
		#parentIds : [
			''816b122accd50eb30738e354ba5270a5426df031''
		]
	},
	HiExampleCommit {
		#id : ''816b122accd50eb30738e354ba5270a5426df031'',
		#comment : ''Allow to correctly check out a tag.\n'',
		#parentIds : [
			''ac915d2c461ae648354afb38b6303ea6ec40870a''
		]
	},
	HiExampleCommit {
		#id : ''8e081d9012907acd7ec853b500eb1e9c1b3355fa'',
		#comment : ''Default to not sharing repositories.\n'',
		#parentIds : [
			''883c2a1d2a3b26c52b43c17334204d55ff31394a''
		]
	},
	HiExampleCommit {
		#id : ''883c2a1d2a3b26c52b43c17334204d55ff31394a'',
		#comment : ''Improvements for #165. Fix cached values.\n'',
		#parentIds : [
			''0be37bd138d0bd94fdf1a2e60ffca92577b68a7a''
		]
	},
	HiExampleCommit {
		#id : ''0be37bd138d0bd94fdf1a2e60ffca92577b68a7a'',
		#comment : ''Fixes #137\n'',
		#parentIds : [
			''1b4d870ef948137cf0eecda45a53e52168ebe3a2''
		]
	},
	HiExampleCommit {
		#id : ''1b4d870ef948137cf0eecda45a53e52168ebe3a2'',
		#comment : ''Added test to verify #185\n'',
		#parentIds : [
			''24a078bbc7505f39614e2380bb9259df3d3fe129''
		]
	},
	HiExampleCommit {
		#id : ''ac915d2c461ae648354afb38b6303ea6ec40870a'',
		#comment : ''Merge branch \''master\'' into dev-0.4\n\nConflicts:\n\tIceberg.package/IceLibgitLocalRepository.class/instance/branch..st\n\tIceberg.package/IceLibgitLocalRepository.class/instance/branch.st\n\tIceberg.package/IceLibgitLocalRepository.class/instance/cloneRepository.st\n\tIceberg.package/IceLibgitLocalRepository.class/instance/currentBranch.st\n\tIceberg.package/IceRemoteScpRepository.class/instance/parseUrl.st\n\tIceberg.package/IceRepository.class/instance/repository..st\n'',
		#parentIds : [
			''03031d01d7c2e4f8aa346478e39365c750666135'',
			''6acc5ab52f1dcd539ba944ab51e83d72f32b2dc6''
		]
	},
	HiExampleCommit {
		#id : ''6acc5ab52f1dcd539ba944ab51e83d72f32b2dc6'',
		#comment : ''Merge pull request #194 from npasserini/dev-0.3\n\nPreview of version 0.3.1'',
		#parentIds : [
			''b265f0f0e0cc8dd9f2ba5ada297dffa6c52f3d50'',
			''24a078bbc7505f39614e2380bb9259df3d3fe129''
		]
	},
	HiExampleCommit {
		#id : ''24a078bbc7505f39614e2380bb9259df3d3fe129'',
		#comment : ''Fixes #191\n'',
		#parentIds : [
			''fce4a8e3e44245027362284bcf10dc01faa83043''
		]
	},
	HiExampleCommit {
		#id : ''fce4a8e3e44245027362284bcf10dc01faa83043'',
		#comment : ''Handle errors when highlighting.\n'',
		#parentIds : [
			''275e669473b41ac9b72173e377bb246bcf6dc96a''
		]
	},
	HiExampleCommit {
		#id : ''275e669473b41ac9b72173e377bb246bcf6dc96a'',
		#comment : ''Improve error handling in view models.\n'',
		#parentIds : [
			''c0a180f2fffbda82005dce0d5e9ab5c5d5dd729b''
		]
	},
	HiExampleCommit {
		#id : ''c0a180f2fffbda82005dce0d5e9ab5c5d5dd729b'',
		#comment : ''Fixed tag tests. Working on #191\n'',
		#parentIds : [
			''5fc9d425c334c69361e0fc98e07cef0d038ecddc''
		]
	},
	HiExampleCommit {
		#id : ''5fc9d425c334c69361e0fc98e07cef0d038ecddc'',
		#comment : ''Fixes #140, branches do not keep the list of commits any more.\n'',
		#parentIds : [
			''65d2f11186cff8cdce7d13718d7dd99543e696a2''
		]
	},
	HiExampleCommit {
		#id : ''65d2f11186cff8cdce7d13718d7dd99543e696a2'',
		#comment : ''Preparing IceRepository for having more kinds of head (instead of always IceBranch).\r\rTo do so, branch creation is delegated to the backend, so that it can create an adecquate object. For example, right now libgit backend provides an IceCommit as head, instead of an IceBranch, when you are in a detached head situation.\r\rThis forces to some added adapters, for having an IceCommit replacing an IceBranch as repository head.\n'',
		#parentIds : [
			''882629bd1b9abc78881015a4cef0d4527992b8cd''
		]
	},
	HiExampleCommit {
		#id : ''882629bd1b9abc78881015a4cef0d4527992b8cd'',
		#comment : ''Let branches handle their own upstreams. Remove upstream method from repository.\n'',
		#parentIds : [
			''a0a074d64ac2149fd7f4d28b8d32d80ced536cfb''
		]
	},
	HiExampleCommit {
		#id : ''a0a074d64ac2149fd7f4d28b8d32d80ced536cfb'',
		#comment : ''Avoid using #codeDirectory from #description because it complaints not having the repository directory before giving the opportunity to create it. \rRemove #commits message from IceRepository.\n'',
		#parentIds : [
			''485705c51798c73c677f2fafef193f83e7797e61''
		]
	},
	HiExampleCommit {
		#id : ''485705c51798c73c677f2fafef193f83e7797e61'',
		#comment : ''Merge branch \''dev-0.3\'' of github.com:npasserini/iceberg into dev-0.3\n'',
		#parentIds : [
			''102b7b1a2f65ba02dc1738ae42e03e0f966d007e'',
			''5d056c14f710ed7e27161faa50ca7cb93d441f4a''
		]
	},
	HiExampleCommit {
		#id : ''102b7b1a2f65ba02dc1738ae42e03e0f966d007e'',
		#comment : ''Avoid requiring the list of all branch commits for saying if a commit is included in a branch or not (IceBranch>>#includesCommit:). Related related to #140.\n'',
		#parentIds : [
			''403331c6b1291889f8e84ac38fb5f2796b89fd56''
		]
	},
	HiExampleCommit {
		#id : ''5d056c14f710ed7e27161faa50ca7cb93d441f4a'',
		#comment : ''Fix code directory for libgit backend\n'',
		#parentIds : [
			''e52858cb02c78e4396b617cd1b67208e588d59cb''
		]
	},
	HiExampleCommit {
		#id : ''e52858cb02c78e4396b617cd1b67208e588d59cb'',
		#comment : ''Fix directory of libgit backends.\n'',
		#parentIds : [
			''ac5c731d867d7df6360620b5778096c3fa76a991''
		]
	},
	HiExampleCommit {
		#id : ''ac5c731d867d7df6360620b5778096c3fa76a991'',
		#comment : ''Fix branch merges: selection is not a string but a branch\n'',
		#parentIds : [
			''78baf30d851a60b6baa623809aa48a4fc64a11ba''
		]
	},
	HiExampleCommit {
		#id : ''78baf30d851a60b6baa623809aa48a4fc64a11ba'',
		#comment : ''Ensure that mergeIntoAnotherBranch provides the good list of branches.\n'',
		#parentIds : [
			''8e47e123160ea5a2d01956f5edb27fdae0064385''
		]
	},
	HiExampleCommit {
		#id : ''8e47e123160ea5a2d01956f5edb27fdae0064385'',
		#comment : ''Class description for Ice libgit backend.\n'',
		#parentIds : [
			''e84523d93019920adb11222ae643dc8dc19f710d''
		]
	},
	HiExampleCommit {
		#id : ''e84523d93019920adb11222ae643dc8dc19f710d'',
		#comment : ''Fix backend selection again\n'',
		#parentIds : [
			''e2119e5efdb72371f725e60810d562131d658315''
		]
	},
	HiExampleCommit {
		#id : ''e2119e5efdb72371f725e60810d562131d658315'',
		#comment : ''Fix for bootstrap: missing #description in libgit backend.\n'',
		#parentIds : [
			''5631efd689ce84dbed068e7e2ae0dc02251ebfbf''
		]
	},
	HiExampleCommit {
		#id : ''5631efd689ce84dbed068e7e2ae0dc02251ebfbf'',
		#comment : ''Merge branch \''master\'' into dev-0.3\n'',
		#parentIds : [
			''8a7a9b98120db6b0c21d9b6cd23493a55c9b1ae3'',
			''b265f0f0e0cc8dd9f2ba5ada297dffa6c52f3d50''
		]
	},
	HiExampleCommit {
		#id : ''8a7a9b98120db6b0c21d9b6cd23493a55c9b1ae3'',
		#comment : ''Fix bug for bootstrap\n'',
		#parentIds : [
			''403331c6b1291889f8e84ac38fb5f2796b89fd56''
		]
	},
	HiExampleCommit {
		#id : ''b265f0f0e0cc8dd9f2ba5ada297dffa6c52f3d50'',
		#comment : ''Fix selection of backend.\n'',
		#parentIds : [
			''5f22c07699fbf111647fc4cceb8479377cd8c6f7''
		]
	},
	HiExampleCommit {
		#id : ''403331c6b1291889f8e84ac38fb5f2796b89fd56'',
		#comment : ''Avoid using a branch\''s commit history (see #140).\n'',
		#parentIds : [
			''7ad96f0ab02d47a210e4d3825ff2b834343f9046''
		]
	},
	HiExampleCommit {
		#id : ''7ad96f0ab02d47a210e4d3825ff2b834343f9046'',
		#comment : ''Even more recovered lost changes.\n'',
		#parentIds : [
			''09417ccee68440ba0672aa7a9aa2af1ebfc93f2b''
		]
	},
	HiExampleCommit {
		#id : ''09417ccee68440ba0672aa7a9aa2af1ebfc93f2b'',
		#comment : ''More recovered lost changes.\n'',
		#parentIds : [
			''e30e9652eb2ff9762b166b2c1a9ff8583d470d49''
		]
	},
	HiExampleCommit {
		#id : ''e30e9652eb2ff9762b166b2c1a9ff8583d470d49'',
		#comment : ''Recover lost changes\n'',
		#parentIds : [
			''c409a15023c39def382fe3be835dd4b8f2d5d665''
		]
	},
	HiExampleCommit {
		#id : ''c409a15023c39def382fe3be835dd4b8f2d5d665'',
		#comment : ''"Merging with 2aa2b1844de2cb44d59f1043b4c5410b70b7ab94"\n'',
		#parentIds : [
			''f0873b55a6f659b4f82373e8b3ced1260dba1539'',
			''2aa2b1844de2cb44d59f1043b4c5410b70b7ab94''
		]
	},
	HiExampleCommit {
		#id : ''f0873b55a6f659b4f82373e8b3ced1260dba1539'',
		#comment : ''Avoid at:ifPresent:ifAbsentPut: to be near to be compatible with Pharo5\n'',
		#parentIds : [
			''03f666f2a5a5b6e9446fb807694d5b8b6308c9d4''
		]
	},
	HiExampleCommit {
		#id : ''03f666f2a5a5b6e9446fb807694d5b8b6308c9d4'',
		#comment : ''Fixes #192\n'',
		#parentIds : [
			''f875cbf4305be6bdc5761c5182a17f739bf1c07c''
		]
	},
	HiExampleCommit {
		#id : ''2aa2b1844de2cb44d59f1043b4c5410b70b7ab94'',
		#comment : ''Fixes #190\n'',
		#parentIds : [
			''6d3ceb18314fd86a8a0106a60ce49495cdebb256''
		]
	},
	HiExampleCommit {
		#id : ''6d3ceb18314fd86a8a0106a60ce49495cdebb256'',
		#comment : ''Introduce temporary versions, to allow handling some requirements while code is being loaded.\rFixed failing tests.\rAdded test for metacello integration.\rExtend interface of IceMetacelloRepositoryAdapter (better metacello integration).\n'',
		#parentIds : [
			''45306f2f28f07899190b1c135aa86cb2d5213552''
		]
	},
	HiExampleCommit {
		#id : ''45306f2f28f07899190b1c135aa86cb2d5213552'',
		#comment : ''Add branch name to commit button.\n'',
		#parentIds : [
			''3aa941c77ffd0c27d931ac33d22de4135fd53bd1''
		]
	},
	HiExampleCommit {
		#id : ''3aa941c77ffd0c27d931ac33d22de4135fd53bd1'',
		#comment : ''New test for metacello integration\n'',
		#parentIds : [
			''5f22c07699fbf111647fc4cceb8479377cd8c6f7''
		]
	},
	HiExampleCommit {
		#id : ''f875cbf4305be6bdc5761c5182a17f739bf1c07c'',
		#comment : ''Introduce  \\"temporary\\" versions, as a place holder for when a version is requested during version load. Refactor versions hierarchy, pull up common methods.\n'',
		#parentIds : [
			''2dca83bb748ce67dacb4ac0c3a50e346895e55e3''
		]
	},
	HiExampleCommit {
		#id : ''2dca83bb748ce67dacb4ac0c3a50e346895e55e3'',
		#comment : ''More debugging info.\n'',
		#parentIds : [
			''4064b2ea0769ca0398cb01f2028d7002730edf0c''
		]
	},
	HiExampleCommit {
		#id : ''4064b2ea0769ca0398cb01f2028d7002730edf0c'',
		#comment : ''Better printOn: for foreign versions. Debugging methods.\n'',
		#parentIds : [
			''171beb484417b7196dce85ccfd4f5ca766c0febe''
		]
	},
	HiExampleCommit {
		#id : ''171beb484417b7196dce85ccfd4f5ca766c0febe'',
		#comment : ''Final (?) step to allow for adding a metacello adapter as a monticello repository.\n'',
		#parentIds : [
			''3173160f82a9bbf27fd07c20626f807f67140fb6''
		]
	},
	HiExampleCommit {
		#id : ''3173160f82a9bbf27fd07c20626f807f67140fb6'',
		#comment : ''Register metacello adapter as a monticello repository for loading packages.\n'',
		#parentIds : [
			''c59963c353db052ee10b7d4ce18d062a958c380c''
		]
	},
	HiExampleCommit {
		#id : ''c59963c353db052ee10b7d4ce18d062a958c380c'',
		#comment : ''Partial improvements for #188\n'',
		#parentIds : [
			''5f22c07699fbf111647fc4cceb8479377cd8c6f7''
		]
	},
	HiExampleCommit {
		#id : ''5f22c07699fbf111647fc4cceb8479377cd8c6f7'',
		#comment : ''Merge pull request #187 from astares/master\n\nChange clone URL so it is easy to remember'',
		#parentIds : [
			''5bed984505d411ce901b1720a6e30687f5f045c8'',
			''355e381abe40276778c37d5fcd80a1b52a59c55f''
		]
	},
	HiExampleCommit {
		#id : ''355e381abe40276778c37d5fcd80a1b52a59c55f'',
		#comment : ''Change clone URL from \\"git@github.com:\\"  into  \\"git@github.com:user/project.git\\" so it is easier to change/remember the format\n'',
		#parentIds : [
			''5bed984505d411ce901b1720a6e30687f5f045c8''
		]
	},
	HiExampleCommit {
		#id : ''5bed984505d411ce901b1720a6e30687f5f045c8'',
		#comment : ''Fixes #182\n'',
		#parentIds : [
			''81fa4a009785b65e73098ee06595724c9ed52823''
		]
	},
	HiExampleCommit {
		#id : ''81fa4a009785b65e73098ee06595724c9ed52823'',
		#comment : ''Fetch all repositories together (see #!82)\n'',
		#parentIds : [
			''359eb7b463d98f2a3c16476da817a3fa8dbed096''
		]
	},
	HiExampleCommit {
		#id : ''359eb7b463d98f2a3c16476da817a3fa8dbed096'',
		#comment : ''Add pull action to update browser.\n'',
		#parentIds : [
			''19b593cb24e9e357f641fe0ed2d5b45f81692c03''
		]
	},
	HiExampleCommit {
		#id : ''19b593cb24e9e357f641fe0ed2d5b45f81692c03'',
		#comment : ''Fix markup to wiki pages.\n'',
		#parentIds : [
			''9b6bbf0636632eb7f989d0fc511f30a584cb58d5''
		]
	},
	HiExampleCommit {
		#id : ''9b6bbf0636632eb7f989d0fc511f30a584cb58d5'',
		#comment : ''Add links to wiki pages.\n'',
		#parentIds : [
			''c4eea05238326b0671fcf24361d2663d315b0296''
		]
	},
	HiExampleCommit {
		#id : ''c4eea05238326b0671fcf24361d2663d315b0296'',
		#comment : ''Fix broken test.\n'',
		#parentIds : [
			''f89e1868c0dd3dfbfce180bd5659da3bbb1f5b3b''
		]
	},
	HiExampleCommit {
		#id : ''f89e1868c0dd3dfbfce180bd5659da3bbb1f5b3b'',
		#comment : ''Fixes #131 - Allow urls with \\".\\"\n'',
		#parentIds : [
			''28bc3e1889414974ff73295742a777e7210ed5e7''
		]
	},
	HiExampleCommit {
		#id : ''28bc3e1889414974ff73295742a777e7210ed5e7'',
		#comment : ''"Merge commit f15a17f669a0dba2be6752c66dcb093eccd40aa3"\n'',
		#parentIds : [
			''290064aa4904a2a77cb010b38fd0752b05b87fc4'',
			''f15a17f669a0dba2be6752c66dcb093eccd40aa3''
		]
	},
	HiExampleCommit {
		#id : ''290064aa4904a2a77cb010b38fd0752b05b87fc4'',
		#comment : ''Fixes #100\n'',
		#parentIds : [
			''13cfbaf9c7507a63655fb1ae6a13385863399f64''
		]
	},
	HiExampleCommit {
		#id : ''f15a17f669a0dba2be6752c66dcb093eccd40aa3'',
		#comment : ''Merge pull request #179 from npasserini/Fixing-shortcuts\n\nChanging the selectionAction and selectionCondition to use enabledConâ\u0080¦'',
		#parentIds : [
			''1d2357f50f486ef199821bb8e9a1627ae0815665'',
			''ec8aa89b764f35f7d49c1b3dbf17a8b16361dfaf''
		]
	},
	HiExampleCommit {
		#id : ''1d2357f50f486ef199821bb8e9a1627ae0815665'',
		#comment : ''Merge pull request #175 from Ducasse/patch-1\n\nUpdate README.md'',
		#parentIds : [
			''13cfbaf9c7507a63655fb1ae6a13385863399f64'',
			''9172f247be0d71c81386e7bce3f0d7048d715664''
		]
	},
	HiExampleCommit {
		#id : ''03031d01d7c2e4f8aa346478e39365c750666135'',
		#comment : ''Save partial non-working changes\n'',
		#parentIds : [
			''b5112a207c5b47cf9e801be0f1e733074ebddbcd''
		]
	},
	HiExampleCommit {
		#id : ''13cfbaf9c7507a63655fb1ae6a13385863399f64'',
		#comment : ''Merge pull request #180 from npasserini/fixing-ssh-key-macos\n\nFixing ssh key macos'',
		#parentIds : [
			''b5112a207c5b47cf9e801be0f1e733074ebddbcd'',
			''677e73d9a77c1ae4eafbd7c53464a597a613bbdf''
		]
	},
	HiExampleCommit {
		#id : ''677e73d9a77c1ae4eafbd7c53464a597a613bbdf'',
		#comment : ''Installing only in osx\n'',
		#parentIds : [
			''d64fd81ae418d6ba9eb2475411d1d918ab448124''
		]
	},
	HiExampleCommit {
		#id : ''d64fd81ae418d6ba9eb2475411d1d918ab448124'',
		#comment : ''Fixing a problem with the dependencies of the new VM in Sierra\n'',
		#parentIds : [
			''6f6e62aee4e161c74207995f888e41838cf73927''
		]
	},
	HiExampleCommit {
		#id : ''6f6e62aee4e161c74207995f888e41838cf73927'',
		#comment : ''Trying to determinate the problem with the osx build\n'',
		#parentIds : [
			''b4bb3daa8ffb14fdb0b2985974d589849268d529''
		]
	},
	HiExampleCommit {
		#id : ''b4bb3daa8ffb14fdb0b2985974d589849268d529'',
		#comment : ''Trying to determinate the problem with the osx build\n'',
		#parentIds : [
			''dd9c02fa9143b47a7c17284d53ed12651642d5f3''
		]
	},
	HiExampleCommit {
		#id : ''dd9c02fa9143b47a7c17284d53ed12651642d5f3'',
		#comment : ''Trying to determinate the problem with the osx build\n'',
		#parentIds : [
			''49fee27cb542d2967cffdd1857822cc3b6d72f9e''
		]
	},
	HiExampleCommit {
		#id : ''49fee27cb542d2967cffdd1857822cc3b6d72f9e'',
		#comment : ''Trying to determinate the problem with the osx build\n'',
		#parentIds : [
			''6c128b30db68c7dddef947bcc908ec123614a575''
		]
	},
	HiExampleCommit {
		#id : ''ec8aa89b764f35f7d49c1b3dbf17a8b16361dfaf'',
		#comment : ''Changing the selectionAction and selectionCondition to use enabledCondition.\rThere is a problem in FastTable, but as soon I can fix it (publish the change) it will be working as a charm\n'',
		#parentIds : [
			''b5112a207c5b47cf9e801be0f1e733074ebddbcd''
		]
	},
	HiExampleCommit {
		#id : ''b5112a207c5b47cf9e801be0f1e733074ebddbcd'',
		#comment : ''"Merge commit 0369ace416da24e04c5c96f80dafad8afbcbfceb"\n'',
		#parentIds : [
			''89a826436df762fb9785468a40788b5721021354'',
			''0369ace416da24e04c5c96f80dafad8afbcbfceb''
		]
	},
	HiExampleCommit {
		#id : ''89a826436df762fb9785468a40788b5721021354'',
		#comment : ''Fixes #178\n'',
		#parentIds : [
			''90fee5db2a0bf7a3aeba32ee9bf38f77a6612688''
		]
	},
	HiExampleCommit {
		#id : ''0369ace416da24e04c5c96f80dafad8afbcbfceb'',
		#comment : ''Merge pull request #177 from npasserini/guillep-patch-readme-branch\n\nFix branch in download script'',
		#parentIds : [
			''90fee5db2a0bf7a3aeba32ee9bf38f77a6612688'',
			''6b21b9834d4e9a9ca8d21a200aa0454c599498df''
		]
	},
	HiExampleCommit {
		#id : ''6b21b9834d4e9a9ca8d21a200aa0454c599498df'',
		#comment : ''Fix branch in download script\n\nReplace download from branch \''stable\'' to default \''master\'' branch. Otherwise people will load old versions.'',
		#parentIds : [
			''90fee5db2a0bf7a3aeba32ee9bf38f77a6612688''
		]
	},
	HiExampleCommit {
		#id : ''90fee5db2a0bf7a3aeba32ee9bf38f77a6612688'',
		#comment : ''"Merge commit 6c128b30db68c7dddef947bcc908ec123614a575"\n'',
		#parentIds : [
			''79970749b3e3385a14706a52d90aec7010235fcb'',
			''6c128b30db68c7dddef947bcc908ec123614a575''
		]
	},
	HiExampleCommit {
		#id : ''79970749b3e3385a14706a52d90aec7010235fcb'',
		#comment : ''Ensure to re-enable epicea after running tests. Avoid error on showing a method from a deleted class in the history.\n'',
		#parentIds : [
			''90700189fff49acc3dc97a21d998d47e1f41473d''
		]
	},
	HiExampleCommit {
		#id : ''6c128b30db68c7dddef947bcc908ec123614a575'',
		#comment : ''Fix link to repositories browser\n'',
		#parentIds : [
			''35e0f759e5f380c078a0e210c9a4b4beb2b1493b''
		]
	},
	HiExampleCommit {
		#id : ''35e0f759e5f380c078a0e210c9a4b4beb2b1493b'',
		#comment : ''Fix link to repositories browser\n'',
		#parentIds : [
			''04cfee96e919d2f70ff7d87216a4418469c63fbd''
		]
	},
	HiExampleCommit {
		#id : ''04cfee96e919d2f70ff7d87216a4418469c63fbd'',
		#comment : ''Add link to repositories browser\n'',
		#parentIds : [
			''352aae77531657d3acc77605d4fc32fddb5e7eae''
		]
	},
	HiExampleCommit {
		#id : ''9172f247be0d71c81386e7bce3f0d7048d715664'',
		#comment : ''Update README.md'',
		#parentIds : [
			''352aae77531657d3acc77605d4fc32fddb5e7eae''
		]
	},
	HiExampleCommit {
		#id : ''352aae77531657d3acc77605d4fc32fddb5e7eae'',
		#comment : ''Avoid using at:ifPresent:ifAbsentPut: for compatibility with Pharo 5.0\n'',
		#parentIds : [
			''3d4ef1d2e158b5cb1f7bac4e0dfcbaa31e8fbf61''
		]
	},
	HiExampleCommit {
		#id : ''3d4ef1d2e158b5cb1f7bac4e0dfcbaa31e8fbf61'',
		#comment : ''Fix test #testVersionFor in travis.\n'',
		#parentIds : [
			''eed2dc3d43aae6de8a4f90970376c04552596698''
		]
	},
	HiExampleCommit {
		#id : ''eed2dc3d43aae6de8a4f90970376c04552596698'',
		#comment : ''Fix travis tests ssh tests attempt #2\n'',
		#parentIds : [
			''d999217e6190eb83cb01f1cac3cff8f408ea13cb''
		]
	},
	HiExampleCommit {
		#id : ''d999217e6190eb83cb01f1cac3cff8f408ea13cb'',
		#comment : ''Try to fix texts for libgit ssh.\n'',
		#parentIds : [
			''90700189fff49acc3dc97a21d998d47e1f41473d''
		]
	},
	HiExampleCommit {
		#id : ''90700189fff49acc3dc97a21d998d47e1f41473d'',
		#comment : ''Fix tests.\n'',
		#parentIds : [
			''a5535d307ca6487480cc4f9061ab4d06d7947c1a''
		]
	},
	HiExampleCommit {
		#id : ''a5535d307ca6487480cc4f9061ab4d06d7947c1a'',
		#comment : ''"Merge commit 06aea22b344043189e2e5a9a1559e605f53d1d36"\n'',
		#parentIds : [
			''9c55232a2dfaa84c7d34d2f877792839952f69f8'',
			''06aea22b344043189e2e5a9a1559e605f53d1d36''
		]
	},
	HiExampleCommit {
		#id : ''9c55232a2dfaa84c7d34d2f877792839952f69f8'',
		#comment : ''"Merge commit 0aa35b445dae42bd1adc97acde126d14624c8860"\n'',
		#parentIds : [
			''0117a96f29550920d2456f70cccaa1793c4ff5b8'',
			''0aa35b445dae42bd1adc97acde126d14624c8860''
		]
	},
	HiExampleCommit {
		#id : ''0117a96f29550920d2456f70cccaa1793c4ff5b8'',
		#comment : ''Fixes #174\n'',
		#parentIds : [
			''cab6c75a06dc0ec7534042eb8c2e50dd8efa750f''
		]
	},
	HiExampleCommit {
		#id : ''06aea22b344043189e2e5a9a1559e605f53d1d36'',
		#comment : ''Move repositories browser section to the wiki\n'',
		#parentIds : [
			''d6574cd7175b8186b7a58fce886ebe39ed91256d''
		]
	},
	HiExampleCommit {
		#id : ''d6574cd7175b8186b7a58fce886ebe39ed91256d'',
		#comment : ''Documentation improvements\n'',
		#parentIds : [
			''0aa35b445dae42bd1adc97acde126d14624c8860''
		]
	},
	HiExampleCommit {
		#id : ''0aa35b445dae42bd1adc97acde126d14624c8860'',
		#comment : ''Merge pull request #172 from npasserini/fixing-the-options-in-icerepository-browser\n\nFixing the adding package option'',
		#parentIds : [
			''cab6c75a06dc0ec7534042eb8c2e50dd8efa750f'',
			''d68a09686125ab6bb6825c821c0532b7edbeae85''
		]
	},
	HiExampleCommit {
		#id : ''d68a09686125ab6bb6825c821c0532b7edbeae85'',
		#comment : ''Fixing the adding package option\n'',
		#parentIds : [
			''cab6c75a06dc0ec7534042eb8c2e50dd8efa750f''
		]
	},
	HiExampleCommit {
		#id : ''cab6c75a06dc0ec7534042eb8c2e50dd8efa750f'',
		#comment : ''Allow to disable metacello integration\n'',
		#parentIds : [
			''27eb18be3b319e77f38576ee18284231fd477cb4''
		]
	},
	HiExampleCommit {
		#id : ''27eb18be3b319e77f38576ee18284231fd477cb4'',
		#comment : ''Avoid having models updating directly the views, use Announcements instead.\n'',
		#parentIds : [
			''14357d86eae047361f76c8e81f5bc7f9971b0be8''
		]
	},
	HiExampleCommit {
		#id : ''14357d86eae047361f76c8e81f5bc7f9971b0be8'',
		#comment : ''Big refactor of all actions in repositories list of repositories browser. Handles the case when a repository directory is missing, and some other errors.\n'',
		#parentIds : [
			''847eec92313f7f76946bc24726be2d606c792cc9''
		]
	},
	HiExampleCommit {
		#id : ''847eec92313f7f76946bc24726be2d606c792cc9'',
		#comment : ''Merge pull request #169 from npasserini/fixing-the-options-in-icerepository-browser\n\nI added a call to repository, because now the object is a model.So aâ\u0080¦'',
		#parentIds : [
			''850d242eefa5bf3e15d72f20afb4ed16d89b964c'',
			''8e33f35691df4105c2a51cffc5b783d88ff5c014''
		]
	},
	HiExampleCommit {
		#id : ''850d242eefa5bf3e15d72f20afb4ed16d89b964c'',
		#comment : ''Do not write repository properties if not needed.\n'',
		#parentIds : [
			''65d93b493116db9d794907b4498c42495faf0b12''
		]
	},
	HiExampleCommit {
		#id : ''8e33f35691df4105c2a51cffc5b783d88ff5c014'',
		#comment : ''I added a call to repository, because now the object is a model.\rSo all the options are working now.\n'',
		#parentIds : [
			''65d93b493116db9d794907b4498c42495faf0b12''
		]
	},
	HiExampleCommit {
		#id : ''65d93b493116db9d794907b4498c42495faf0b12'',
		#comment : ''Fix for loading / unloading specific packages.\n'',
		#parentIds : [
			''cd9230e13c5d54aa92276f0f8376c40a68cdae58''
		]
	},
	HiExampleCommit {
		#id : ''cd9230e13c5d54aa92276f0f8376c40a68cdae58'',
		#comment : ''Avoid using Iceberg to load iceberg itself from metacello (see #168)\n'',
		#parentIds : [
			''f65c78d74aacb53b3f1bc5b730d3094dd97f87b3''
		]
	},
	HiExampleCommit {
		#id : ''f65c78d74aacb53b3f1bc5b730d3094dd97f87b3'',
		#comment : ''Fixed forgetRepository + small refactors.\n'',
		#parentIds : [
			''1b8bed0f9c858a935a9aaaeb42d9b36e2e97f587''
		]
	},
	HiExampleCommit {
		#id : ''1b8bed0f9c858a935a9aaaeb42d9b36e2e97f587'',
		#comment : ''Log the name of the repository being created.\n'',
		#parentIds : [
			''da0c2508e41aac8a2053321a1830a07664875b70''
		]
	},
	HiExampleCommit {
		#id : ''da0c2508e41aac8a2053321a1830a07664875b70'',
		#comment : ''Caching values to minimize amount of redraus (see #165)\n'',
		#parentIds : [
			''b0835638fa2f0068b981b16bd6db13d3d6a84d71''
		]
	},
	HiExampleCommit {
		#id : ''b0835638fa2f0068b981b16bd6db13d3d6a84d71'',
		#comment : ''Create models to handle communication between repositories browser and ice repositories. Models will be responsible for caching values and updating gui accordingily.\n'',
		#parentIds : [
			''53c26c36d42565138e02f05081c67c66fdaeef8b''
		]
	},
	HiExampleCommit {
		#id : ''53c26c36d42565138e02f05081c67c66fdaeef8b'',
		#comment : ''Packages loaded using metacello+iceberg should be traceable to iceberg versions.\n'',
		#parentIds : [
			''d8ba9d745d1d505dd868250191a2598222b5c198''
		]
	},
	HiExampleCommit {
		#id : ''d8ba9d745d1d505dd868250191a2598222b5c198'',
		#comment : ''Create adapters for allow Metacello to use Iceberg to download packages.\rMethod #getOrCreateIcebergRepository should should for an existing repository before creating a new one.\rFix #printOn: in IceClassChangeSet, we should not send #name to classes, use #className instead.\n'',
		#parentIds : [
			''97f6a89f70fad5aa753521da7e28fda0619256f7''
		]
	},
	HiExampleCommit {
		#id : ''97f6a89f70fad5aa753521da7e28fda0619256f7'',
		#comment : ''Fixed bug while loading packages.\n'',
		#parentIds : [
			''2f746b98bf92934dcc0f52c73670bca69103dc11''
		]
	},
	HiExampleCommit {
		#id : ''2f746b98bf92934dcc0f52c73670bca69103dc11'',
		#comment : ''Add incorrectly packaged IceRepositoryAnnouncement. Add tests to ensure that empty classes are saved correctly.\n'',
		#parentIds : [
			''2b3241b7218a8db137335830a625815113c58e5b''
		]
	},
	HiExampleCommit {
		#id : ''2b3241b7218a8db137335830a625815113c58e5b'',
		#comment : ''Fixes #117, fixes #132. Improve foreign versions polymorphism with ice versions.\n'',
		#parentIds : [
			''6ed0f67ee328d9d72f3745ea308052a9c716a17e''
		]
	},
	HiExampleCommit {
		#id : ''6ed0f67ee328d9d72f3745ea308052a9c716a17e'',
		#comment : ''Fixes #161 - IceRemoteFileRepository should understand host?\n'',
		#parentIds : [
			''a429667b301d26a551837757b9bbc039daab7f7a''
		]
	},
	HiExampleCommit {
		#id : ''a429667b301d26a551837757b9bbc039daab7f7a'',
		#comment : ''Include versionFor: in saved package.\n'',
		#parentIds : [
			''f68787b8283b8d8b56ef2b60067c389c035307d4''
		]
	},
	HiExampleCommit {
		#id : ''f68787b8283b8d8b56ef2b60067c389c035307d4'',
		#comment : ''Avoid addAll to the index, and select changed directories instead.\n'',
		#parentIds : [
			''524cbfbd30de77514b772edfa15f4f12083dad68''
		]
	},
	HiExampleCommit {
		#id : ''524cbfbd30de77514b772edfa15f4f12083dad68'',
		#comment : ''Retry abstract announcement.\n'',
		#parentIds : [
			''7d64f4f5e207c24da9992373a8ac161729c555f4''
		]
	},
	HiExampleCommit {
		#id : ''7d64f4f5e207c24da9992373a8ac161729c555f4'',
		#comment : ''Back to announcement set, instead of abstract announcements.\n'',
		#parentIds : [
			''56a2c7ec166e2ab8d35da6acdf35eda7f79b812b''
		]
	},
	HiExampleCommit {
		#id : ''56a2c7ec166e2ab8d35da6acdf35eda7f79b812b'',
		#comment : ''Create superclass for repository announcements.\n'',
		#parentIds : [
			''4feed5629f5a720921638bea1e6edff2b37e1da4''
		]
	},
	HiExampleCommit {
		#id : ''4feed5629f5a720921638bea1e6edff2b37e1da4'',
		#comment : ''Minor fix.\n'',
		#parentIds : [
			''30010896c5eec8a5084546616ee873fbb2612735''
		]
	},
	HiExampleCommit {
		#id : ''30010896c5eec8a5084546616ee873fbb2612735'',
		#comment : ''Fixes #164. Improve history browser, show diff between working copy and selected commit.\n'',
		#parentIds : [
			''3624cd9a5baf3627858b609ea35db817841702ff''
		]
	},
	HiExampleCommit {
		#id : ''3624cd9a5baf3627858b609ea35db817841702ff'',
		#comment : ''Remove code duplication.\n'',
		#parentIds : [
			''65434067310874c4edcf5854ccb1fc292b50ef33''
		]
	},
	HiExampleCommit {
		#id : ''65434067310874c4edcf5854ccb1fc292b50ef33'',
		#comment : ''Add listening to push events.\n'',
		#parentIds : [
			''feba2f19210d1871ab52f6362f46863285dece3d''
		]
	},
	HiExampleCommit {
		#id : ''feba2f19210d1871ab52f6362f46863285dece3d'',
		#comment : ''Notify monticello when I realise that a package has no changes.\n'',
		#parentIds : [
			''677f3ce44c1d5f3115c11e6cefe1fc9cfa2b1808''
		]
	},
	HiExampleCommit {
		#id : ''677f3ce44c1d5f3115c11e6cefe1fc9cfa2b1808'',
		#comment : ''Have repositories browser listening to commit events. Fixes #130\n'',
		#parentIds : [
			''a53b878ead753cf40f71503339cb4697e47b803e''
		]
	},
	HiExampleCommit {
		#id : ''a53b878ead753cf40f71503339cb4697e47b803e'',
		#comment : ''Fixes #163\n'',
		#parentIds : [
			''47dadda71e9058168cc81cb3ee4878f73edbb3cb''
		]
	},
	HiExampleCommit {
		#id : ''47dadda71e9058168cc81cb3ee4878f73edbb3cb'',
		#comment : ''Fixes #!62: Add highlighting to packages and repositories with pending actions.\rRepository and package now understand several common messages. \r#incomming/#outgoingVersions renamed to #incomming/#outgoingCommits\n'',
		#parentIds : [
			''6430ee2dd7d31b3343f8758503ca6ffb6f2de63a''
		]
	},
	HiExampleCommit {
		#id : ''6430ee2dd7d31b3343f8758503ca6ffb6f2de63a'',
		#comment : ''Fix tests for libgit backend\r- add merge capabilities\r- commits created by the backend must allways have a reference to the frontend.\n'',
		#parentIds : [
			''72ba59cb817fe684e0d93bac512cd6b12d619b47''
		]
	},
	HiExampleCommit {
		#id : ''72ba59cb817fe684e0d93bac512cd6b12d619b47'',
		#comment : ''Enhance libgit commit walk with support for limiting the number of results.\n'',
		#parentIds : [
			''59e9f0f388f536478a23da08d9b55f7b4fd4e6ea''
		]
	},
	HiExampleCommit {
		#id : ''59e9f0f388f536478a23da08d9b55f7b4fd4e6ea'',
		#comment : ''Adapt IceLibgitLocalRepository to the new IceRepository implementation, namely add internalStoreVersion: and commit:withMessage:.\r\rAlso:\r- IceBackendWriterAdapter allows to use a MCFileTreeWriter to write packages on to an Iceberg backend.\r- Last commit from a branch does not load all commits for the branch.\r- IceLibgitCommitWalk is now capable of receiving  IceBranches that do not point to a real branch but to whatever commit-ish expression.\r- Fixes several libgit tests.\n'',
		#parentIds : [
			''b3edb3d7059470c158e24f7ff72e8da384818bca''
		]
	},
	HiExampleCommit {
		#id : ''b3edb3d7059470c158e24f7ff72e8da384818bca'',
		#comment : ''"Merge commit 3014717652476befc71b007942b69926d568c6de"\n'',
		#parentIds : [
			''2e2144f699c3f9609073c4ddc5becf8fc81ec141'',
			''3014717652476befc71b007942b69926d568c6de''
		]
	},
	HiExampleCommit {
		#id : ''2e2144f699c3f9609073c4ddc5becf8fc81ec141'',
		#comment : ''Refactored IceSavedPackageVersion => I still have origination commit, but the loaded commit is the most important concept.\r\rAnd then:\r- Many changes due to the interface change.\r- IceCommitInfo#load does not fire announcements because they will be fired again by IceLoadedCode\r- IceLoadedCode#commitsNotLoaded uses reference commit, instead of loaded versions. This requires still more improvements.\r- Tests delete also gitfiletree temp directories\n'',
		#parentIds : [
			''e3d2f6d2e5654c46b99ef550b39a205f01b2d918''
		]
	},
	HiExampleCommit {
		#id : ''3014717652476befc71b007942b69926d568c6de'',
		#comment : ''Merge pull request #158 from npasserini/adding-ssh-key-for-test\n\nAdding encrypted key'',
		#parentIds : [
			''e3d2f6d2e5654c46b99ef550b39a205f01b2d918'',
			''6f8a0e4dd0d5ae880fc4b47053ced09ad503d347''
		]
	},
	HiExampleCommit {
		#id : ''e3d2f6d2e5654c46b99ef550b39a205f01b2d918'',
		#comment : ''Compute loaded versions for gitfiletree repositories.\nAlso:\n- IceLoadedCode computes automatically loaded versions.\n- Reference commit takes earliest commit in case of not being able to determine a single one... this is still partial implementation, needs more improvements.\n- Some improvements to IceForeignVersion, in the path to make it fully polymorphic with normal versions. Some tasks have to be delegated to the versions to allow them to act polymorphically.\n'',
		#parentIds : [
			''179e735c3a86797a3775abf0e9a90f5fd00b1e7a''
		]
	},
	HiExampleCommit {
		#id : ''6f8a0e4dd0d5ae880fc4b47053ced09ad503d347'',
		#comment : ''Adding encrypted key\n'',
		#parentIds : [
			''8e26b62d739a8de6584dbd4eca645fe810064154''
		]
	},
	HiExampleCommit {
		#id : ''179e735c3a86797a3775abf0e9a90f5fd00b1e7a'',
		#comment : ''Save code in any state because image is broken.\n'',
		#parentIds : [
			''8e26b62d739a8de6584dbd4eca645fe810064154''
		]
	},
	HiExampleCommit {
		#id : ''8e26b62d739a8de6584dbd4eca645fe810064154'',
		#comment : ''Fixing tests for Libgit\r- add fromBranch: to IceLibgitCommitWalk\r- add fetch to IceLibgitLocalRepository\n'',
		#parentIds : [
			''7385deef67b9db41f53b790ffb5b657941e50c75''
		]
	},
	HiExampleCommit {
		#id : ''7385deef67b9db41f53b790ffb5b657941e50c75'',
		#comment : ''"Merge commit 6101b259bf826c180709c18c0e287567689aae5b"\n'',
		#parentIds : [
			''e1dbd390c88ebbb9628813e0bc4cf7782a25ed4a'',
			''6101b259bf826c180709c18c0e287567689aae5b''
		]
	},
	HiExampleCommit {
		#id : ''e1dbd390c88ebbb9628813e0bc4cf7782a25ed4a'',
		#comment : ''Fix commit creation, was hard coded to use gitfiletree version.\n'',
		#parentIds : [
			''745aa7dadcd19b6578c2982dcabe06def3ea794f''
		]
	},
	HiExampleCommit {
		#id : ''745aa7dadcd19b6578c2982dcabe06def3ea794f'',
		#comment : ''Do not load commits for current branch & upstream every time someone asks for the commit dictionary of a repository.\rCreate commit walk for libgit backends. Move sharable code to IceAbstractCommitWalk.\n'',
		#parentIds : [
			''94147e3fc2b1b025e02d2f8aa3617020f2782023''
		]
	},
	HiExampleCommit {
		#id : ''6101b259bf826c180709c18c0e287567689aae5b'',
		#comment : ''Fix IceGUITest to be able to run in travis\n'',
		#parentIds : [
			''566d392002201d6ae741dde40968a9829ad40015''
		]
	},
	HiExampleCommit {
		#id : ''566d392002201d6ae741dde40968a9829ad40015'',
		#comment : ''Log while setting git global user.\n'',
		#parentIds : [
			''94147e3fc2b1b025e02d2f8aa3617020f2782023''
		]
	},
	HiExampleCommit {
		#id : ''94147e3fc2b1b025e02d2f8aa3617020f2782023'',
		#comment : ''Fix how we set global variables to initialize git author information in CI\n'',
		#parentIds : [
			''b03c635c26172f68b9d6343c280caf0005033db8''
		]
	},
	HiExampleCommit {
		#id : ''b03c635c26172f68b9d6343c280caf0005033db8'',
		#comment : ''Use environment variables to have special way or running tests in CI. Use it to initialize git user data.\n'',
		#parentIds : [
			''0344fd309e2b380a6dddc2e47922c560906808e1''
		]
	},
	HiExampleCommit {
		#id : ''0344fd309e2b380a6dddc2e47922c560906808e1'',
		#comment : ''Fix test setup\n'',
		#parentIds : [
			''f91a9b155c7978721482ced76010340812f1a3b2''
		]
	},
	HiExampleCommit {
		#id : ''f91a9b155c7978721482ced76010340812f1a3b2'',
		#comment : ''Fix bug in tests setup.\n'',
		#parentIds : [
			''a0499beb4640f7732195d6cf3bf35117cd6f0dfd''
		]
	},
	HiExampleCommit {
		#id : ''a0499beb4640f7732195d6cf3bf35117cd6f0dfd'',
		#comment : ''Debugging travis test run: log errors on #runCase\n'',
		#parentIds : [
			''68ee2cebb331d4da40212ea18779b0b9e051849c''
		]
	},
	HiExampleCommit {
		#id : ''68ee2cebb331d4da40212ea18779b0b9e051849c'',
		#comment : ''Add transcript logging to see where do travis tests fail.\n'',
		#parentIds : [
			''0950e5bc42f877369a53734ac1464804c7a9b0b7''
		]
	},
	HiExampleCommit {
		#id : ''0950e5bc42f877369a53734ac1464804c7a9b0b7'',
		#comment : ''Fixing tests for travis and libgit.\n'',
		#parentIds : [
			''741d3e5d23400a08e718d3489c5fab3f56b824bb''
		]
	},
	HiExampleCommit {
		#id : ''741d3e5d23400a08e718d3489c5fab3f56b824bb'',
		#comment : ''Change clone url for tests, use https for the first clone, easier to make it run in travis.\n'',
		#parentIds : [
			''403b91d1e123bc8289d8fcd1a5e19b79124be179''
		]
	},
	HiExampleCommit {
		#id : ''403b91d1e123bc8289d8fcd1a5e19b79124be179'',
		#comment : ''Prevent #tearDown to fail when the precedent test failed to create the repository.\n'',
		#parentIds : [
			''6d1cc06298e41c7e2d82a992a41154bb746926ca''
		]
	},
	HiExampleCommit {
		#id : ''6d1cc06298e41c7e2d82a992a41154bb746926ca'',
		#comment : ''First try to run tests on travis.\n'',
		#parentIds : [
			''35dcf0dd15bd77f585197566ac396ce18bf16763''
		]
	},
	HiExampleCommit {
		#id : ''35dcf0dd15bd77f585197566ac396ce18bf16763'',
		#comment : ''"Merge commit 7d45ed31bd8b7d06b260919ffecdb73c9a1a85e0"\n'',
		#parentIds : [
			''a773e52a1dc4340f1e75e7477dd7b14ae5bfb9d4'',
			''7d45ed31bd8b7d06b260919ffecdb73c9a1a85e0''
		]
	},
	HiExampleCommit {
		#id : ''a773e52a1dc4340f1e75e7477dd7b14ae5bfb9d4'',
		#comment : ''Fixes #153\n'',
		#parentIds : [
			''da99a0f1ba299c86df33a5f5e7cc65f53c8f7f81''
		]
	},
	HiExampleCommit {
		#id : ''7d45ed31bd8b7d06b260919ffecdb73c9a1a85e0'',
		#comment : ''Change travis configuration to ensure that tests are loaded\n'',
		#parentIds : [
			''7ce3b89a5f4e043fcfe6090a98fce3a589ad5c75''
		]
	},
	HiExampleCommit {
		#id : ''7ce3b89a5f4e043fcfe6090a98fce3a589ad5c75'',
		#comment : ''Change travis configuration to ensure that tests are loaded\n'',
		#parentIds : [
			''da99a0f1ba299c86df33a5f5e7cc65f53c8f7f81''
		]
	},
	HiExampleCommit {
		#id : ''da99a0f1ba299c86df33a5f5e7cc65f53c8f7f81'',
		#comment : ''"Merge commit f2a329b332fb7d8422c8fe999571103d00e19143"\n'',
		#parentIds : [
			''f65fdd25e1906d1185aecff7fd66192bb5997970'',
			''f2a329b332fb7d8422c8fe999571103d00e19143''
		]
	},
	HiExampleCommit {
		#id : ''f65fdd25e1906d1185aecff7fd66192bb5997970'',
		#comment : ''Fixes: #152: Bad handling of rejected pushes.\n'',
		#parentIds : [
			''df1f4fa41d28f5aedc68a3b888b46b125caf81fb''
		]
	},
	HiExampleCommit {
		#id : ''df1f4fa41d28f5aedc68a3b888b46b125caf81fb'',
		#comment : ''Fixes: #151 - Correctly show diffs for incomming issues.\n'',
		#parentIds : [
			''c65a6c4ed0d6132a9af08a29dd95abed2489d176''
		]
	},
	HiExampleCommit {
		#id : ''f2a329b332fb7d8422c8fe999571103d00e19143'',
		#comment : ''Fixes #110 - Update commit message after commit.\n'',
		#parentIds : [
			''fc344aa28b012cada10beb46eeea9104bc6bf996''
		]
	},
	HiExampleCommit {
		#id : ''fc344aa28b012cada10beb46eeea9104bc6bf996'',
		#comment : ''Clean commit message after commiting.\n'',
		#parentIds : [
			''3649f7199ce5139cad27c4d92a50591975ec1950''
		]
	},
	HiExampleCommit {
		#id : ''3649f7199ce5139cad27c4d92a50591975ec1950'',
		#comment : ''Improvements to #110, diff view will be reset after commit.\n\nIn fact it will be reset after any change to the changes tree, which is not great but at least is consistent.\n'',
		#parentIds : [
			''c65a6c4ed0d6132a9af08a29dd95abed2489d176''
		]
	},
	HiExampleCommit {
		#id : ''c65a6c4ed0d6132a9af08a29dd95abed2489d176'',
		#comment : ''Fix diff-tree menu for packages.\n'',
		#parentIds : [
			''d8ede1e138d3b00aa62531a98414471cd3210f14''
		]
	},
	HiExampleCommit {
		#id : ''d8ede1e138d3b00aa62531a98414471cd3210f14'',
		#comment : ''Fixing broken tests:\r- do not try to compute reference commit on initialization, because it depends on other stuff which is not ready.\r- update loaded code after merge commits (and refactor commit messages to ensure that everything is done consistenly).\n'',
		#parentIds : [
			''563f34384660467cb24d7faec126b999ab843e55''
		]
	},
	HiExampleCommit {
		#id : ''563f34384660467cb24d7faec126b999ab843e55'',
		#comment : ''Fixes #146 - Show correct diffs for incomming commits.\rAlso:\r-  separate \\"load\\" (for not loaded commits that are already merged into the present branch) and  \\"merge\\" (for commits that come from another branch).\r- show commit status in the incomming commit list.\r- fix #commitsNotLoaded, to look for updation commit instead of just #commit\n'',
		#parentIds : [
			''ed419bb81996cb15b1f2acd548ace6c2136346e0''
		]
	},
	HiExampleCommit {
		#id : ''ed419bb81996cb15b1f2acd548ace6c2136346e0'',
		#comment : ''Fixed creation of diff, do not create a new instance of loaded code, use that one from the repository.\n'',
		#parentIds : [
			''c5f86328a478897e5724a5ca77064422cb25951d''
		]
	},
	HiExampleCommit {
		#id : ''c5f86328a478897e5724a5ca77064422cb25951d'',
		#comment : ''Some refactorings improving how we build diffs.\rSlowly we should be near being able to compute diffs between any two \''commitish\''\n'',
		#parentIds : [
			''41081dc50346f2d652c11af6fd7e538e258405f2''
		]
	},
	HiExampleCommit {
		#id : ''41081dc50346f2d652c11af6fd7e538e258405f2'',
		#comment : ''Fixes #46 - Show diff in push browser.\rAlso fix a bug while building options menu for a method in a diff tree.\n'',
		#parentIds : [
			''8610da1ab1cb853e61229f1f044163112c6b46a2''
		]
	},
	HiExampleCommit {
		#id : ''8610da1ab1cb853e61229f1f044163112c6b46a2'',
		#comment : ''Fixes #145 (main issue was already finished, here there is just some cleanup)\rReplaced IceCurrentHead for IceLoadedCode in main diff view.\rImproved class comments.\n'',
		#parentIds : [
			''d049e14d99ed29eab99d2083365a9778d7f67d2d''
		]
	},
	HiExampleCommit {
		#id : ''d049e14d99ed29eab99d2083365a9778d7f67d2d'',
		#comment : ''Add option to browse class references from changes tree.\rReplace IceCurrentHead for IceLoadedCode in default changes tree.\rSome clean up related to loaded code (see #145)\n'',
		#parentIds : [
			''0a7814a893745bdc6989cb92a78b2e7c81ec205b''
		]
	},
	HiExampleCommit {
		#id : ''0a7814a893745bdc6989cb92a78b2e7c81ec205b'',
		#comment : ''Fixes: #142, cleanup of old ways of getting versions for a commit.\n'',
		#parentIds : [
			''3dd97bb70d065ea76cda16917e12192233dba8b4''
		]
	},
	HiExampleCommit {
		#id : ''3dd97bb70d065ea76cda16917e12192233dba8b4'',
		#comment : ''Remove properVersionFor: from IceCommitInfo, partial fix for #142.\rDo not show browse option in changes tree for deleted methods (improvement on #66)\n'',
		#parentIds : [
			''6537015b1e871da0fa10c3eddcbe35b26ff40f4f''
		]
	},
	HiExampleCommit {
		#id : ''6537015b1e871da0fa10c3eddcbe35b26ff40f4f'',
		#comment : ''Fixes #147, improved performance for several operations.\n'',
		#parentIds : [
			''347ffb64e594458a0c8c90e7eb0bc79f9c23f119''
		]
	},
	HiExampleCommit {
		#id : ''347ffb64e594458a0c8c90e7eb0bc79f9c23f119'',
		#comment : ''A commit should always know its repository.\n'',
		#parentIds : [
			''fee6c5895107796f05e6fe90f4efd34b7a6e473a''
		]
	},
	HiExampleCommit {
		#id : ''fee6c5895107796f05e6fe90f4efd34b7a6e473a'',
		#comment : ''Fixes #66 - Allow to browse from the commit view\n'',
		#parentIds : [
			''d92bf7af861e0ecb9c69f83d40299d654a6e8b20''
		]
	},
	HiExampleCommit {
		#id : ''d92bf7af861e0ecb9c69f83d40299d654a6e8b20'',
		#comment : ''Fixes #135, changes tree is refreshed after commiting.\n'',
		#parentIds : [
			''ef08c2013fa8bb0407f47aada377f41065b358cd''
		]
	},
	HiExampleCommit {
		#id : ''ef08c2013fa8bb0407f47aada377f41065b358cd'',
		#comment : ''Update changes tree on commit. Partial fix for #135.\n'',
		#parentIds : [
			''3b7d23b021af36813811c5fd81f4b5288fee3459''
		]
	},
	HiExampleCommit {
		#id : ''3b7d23b021af36813811c5fd81f4b5288fee3459'',
		#comment : ''Some cleanup, remove unused methods.\n'',
		#parentIds : [
			''8c2cc972c2cd310398d69ded82cf303026d659f2''
		]
	},
	HiExampleCommit {
		#id : ''8c2cc972c2cd310398d69ded82cf303026d659f2'',
		#comment : ''Add some checks before adding a new commit to a branch (might be removed later, this is a temporal extra-security, until we finish reengineering).\nSimplify version loading, now the branch loads only versions for the requested package.\nCreated IceForeignVersion, which is a placeholder for when we can not relate the currently loaded version with an Iceberg version.\nAdd utility maintenance method, to rebuild repositories.\nClean up, delete old unused code.\n'',
		#parentIds : [
			''e671638a7bae978bb682d163948ab5d92524906b''
		]
	},
	HiExampleCommit {
		#id : ''e671638a7bae978bb682d163948ab5d92524906b'',
		#comment : ''Alert after commit.\n'',
		#parentIds : [
			''66726bc63a14e82b37f5097a3dce0d914edfa3e7''
		]
	},
	HiExampleCommit {
		#id : ''66726bc63a14e82b37f5097a3dce0d914edfa3e7'',
		#comment : ''Update package status #2\n'',
		#parentIds : [
			''76ae5dd34c1d0ac96547b0c1bf2996b3bac48aca''
		]
	},
	HiExampleCommit {
		#id : ''76ae5dd34c1d0ac96547b0c1bf2996b3bac48aca'',
		#comment : ''Update package status.\n'',
		#parentIds : [
			''8ebab48fb4a81c36461307d77800e7eb26acce3b''
		]
	},
	HiExampleCommit {
		#id : ''8ebab48fb4a81c36461307d77800e7eb26acce3b'',
		#comment : ''Fix announcement registration.\n'',
		#parentIds : [
			''b59764c1078f0bd5e9795074fef41642a4706b31''
		]
	},
	HiExampleCommit {
		#id : ''b59764c1078f0bd5e9795074fef41642a4706b31'',
		#comment : ''Have synchronizer listen to announcement from the repository.\n'',
		#parentIds : [
			''ae3f3560cf20be64934d9db3c591f85bd4c76ed1''
		]
	},
	HiExampleCommit {
		#id : ''ae3f3560cf20be64934d9db3c591f85bd4c76ed1'',
		#comment : ''Update loadedCode after a commit.\n'',
		#parentIds : [
			''0a0f4202d5b41f0597f2e2a0abbfeed48223715b''
		]
	},
	HiExampleCommit {
		#id : ''0a0f4202d5b41f0597f2e2a0abbfeed48223715b'',
		#comment : ''Now commit walks allways gather all information of a commit and add it to a cache before returning it. It does not make sense to return ids, allways return real commits.\r\rInitial intention of IceLoadedCode keeping a  \''reference commit\''\r\rIceMCRepository allways registers new commits into the current branch.\r\rOn save package update loaded code with the new commit info.\n'',
		#parentIds : [
			''c67d3ab72b9afb42eef098299e9cd654d371ebdf''
		]
	},
	HiExampleCommit {
		#id : ''c67d3ab72b9afb42eef098299e9cd654d371ebdf'',
		#comment : ''Fixed test\n'',
		#parentIds : [
			''c1ba35df4e0324462be443224fb82af5442e40e9''
		]
	},
	HiExampleCommit {
		#id : ''c1ba35df4e0324462be443224fb82af5442e40e9'',
		#comment : ''Integrate new loaded code objects with the old loadedVersion from packages.\n'',
		#parentIds : [
			''ba7ab0f537692d0fa470e287e61654112de5daee''
		]
	},
	HiExampleCommit {
		#id : ''ba7ab0f537692d0fa470e287e61654112de5daee'',
		#comment : ''Fixed testLoadCommit\n'',
		#parentIds : [
			''4759db8270f612999c478884e211ccfca51b8de2''
		]
	},
	HiExampleCommit {
		#id : ''4759db8270f612999c478884e211ccfca51b8de2'',
		#comment : ''New \''commit walk\'' allows for simpler looking for commits and older versions.\rIceSavedPackageVersion now looks for info lazily (to be tested!).\rSimplify the gathering of loaded versions.\rImproved tests.\n'',
		#parentIds : [
			''ddfe192c2bc737b78ffc48fbc97c8da03205c704''
		]
	},
	HiExampleCommit {
		#id : ''ddfe192c2bc737b78ffc48fbc97c8da03205c704'',
		#comment : ''First steps into a big refactor.\r- Created three code status: no code loaded / homogeneous code loaded / heterogeneus code loaded\r- Now the IceMCGitRepository knows its \\"frontend\\" (an IceRepository), it should be done also for the LibGitVersion.\r- Slowly removing the version management, focus on commit management. Will see if versions are still needed in some situations or we can get rid of them completely.\r\rOptimization\r- Commits now add the created commit to the repository without the need for refresh.\r\rCleanup\r- Remove unused #commit method in IceMCGitRepository\r- Implemented own #internalStoreVersion, slowly decoupling from gitfiletree\r-\n'',
		#parentIds : [
			''9ef38265f2fd94c2e36629763624c5500feb59eb''
		]
	},
	HiExampleCommit {
		#id : ''9ef38265f2fd94c2e36629763624c5500feb59eb'',
		#comment : ''Add a merge button to the history browser.\n'',
		#parentIds : [
			''b2b39d9ddda0e469a606eebf1afeaba87118047d''
		]
	},
	HiExampleCommit {
		#id : ''b2b39d9ddda0e469a606eebf1afeaba87118047d'',
		#comment : ''Add an inform dialog while pushing.\n'',
		#parentIds : [
			''79d2bd44b9a73ee458438ba3d5490b8407c8605d''
		]
	},
	HiExampleCommit {
		#id : ''79d2bd44b9a73ee458438ba3d5490b8407c8605d'',
		#comment : ''Fix a bug in the push action from the repositories list.\n'',
		#parentIds : [
			''a4d8887b5a00f69a94c603aef121c0a131e5479c''
		]
	},
	HiExampleCommit {
		#id : ''a4d8887b5a00f69a94c603aef121c0a131e5479c'',
		#comment : ''Fixes #3: Merge a branch into another one.\n'',
		#parentIds : [
			''f97a8810581fe78f5bc313dc5d23a9257b1ff81f''
		]
	},
	HiExampleCommit {
		#id : ''f97a8810581fe78f5bc313dc5d23a9257b1ff81f'',
		#comment : ''When a branch has now upstream, show as  \\"outgoing\\" all (versions corresponding to a) commit which is not in any other branch.\n'',
		#parentIds : [
			''d5daf7e45d888bfcde4d1d2c9d2e9da5c494454b''
		]
	},
	HiExampleCommit {
		#id : ''d5daf7e45d888bfcde4d1d2c9d2e9da5c494454b'',
		#comment : ''\\"Create new branch\\" feature. Improve status for \\"outgoing\\" when upstream does not exist.\n'',
		#parentIds : [
			''feb07b6848b23af6e086df31eba84772c9730357''
		]
	},
	HiExampleCommit {
		#id : ''feb07b6848b23af6e086df31eba84772c9730357'',
		#comment : ''Fixes #134, update gitfiletree on  \\"Iceberg update\\" and fixes #133, auto detect subdirectory when creating an IceRepo out of an MCGithubRepo.\rPartial fix to #132, DNU will not arise but we still need a refactor to have everything in order.\rNew method properVersion, different from version. Only proper version will look for a version changed in the selected commit.\rAdding version comparison to publish view, still work in progress.\r#loadedVersion in IceSavedPackage fixes an error in showing how many incomming commits we have.\n'',
		#parentIds : [
			''4394aecdbe055a415b75e195b07f8acf4a25ea09''
		]
	},
	HiExampleCommit {
		#id : ''4394aecdbe055a415b75e195b07f8acf4a25ea09'',
		#comment : ''"Merge commit 276d7bb21b673635e0b15bc8e769948fc92a26b7"\n'',
		#parentIds : [
			''4199083367e9e7904d9c75ee8831b8667ce6a019'',
			''276d7bb21b673635e0b15bc8e769948fc92a26b7''
		]
	},
	HiExampleCommit {
		#id : ''4199083367e9e7904d9c75ee8831b8667ce6a019'',
		#comment : ''Fixes #126, previous fix did not take into account all possible ways of pushing.\rCreated TIceTool to share utility code between Ice Glamour- and Spec-based tools.\n'',
		#parentIds : [
			''034fa4008b7edb107041c89bef09e2ba55dea881''
		]
	},
	HiExampleCommit {
		#id : ''034fa4008b7edb107041c89bef09e2ba55dea881'',
		#comment : ''Fixes #126, handle failed pull attempts and show a nicer message.\n'',
		#parentIds : [
			''39c8a3888b8f66feb9f4f9464a20372133160c76''
		]
	},
	HiExampleCommit {
		#id : ''39c8a3888b8f66feb9f4f9464a20372133160c76'',
		#comment : ''Update view after adding a package. Fix bug in repo initialisation.\n'',
		#parentIds : [
			''a0dc80eeb2e8b586d2235e754a99b70f1db8e4ae''
		]
	},
	HiExampleCommit {
		#id : ''276d7bb21b673635e0b15bc8e769948fc92a26b7'',
		#comment : ''Merge pull request #121 from StephanEggermont/patch-1\n\nTypos'',
		#parentIds : [
			''a0dc80eeb2e8b586d2235e754a99b70f1db8e4ae'',
			''41cf141be4840154997fe687e2f35e9b67393f1d''
		]
	},
	HiExampleCommit {
		#id : ''a0dc80eeb2e8b586d2235e754a99b70f1db8e4ae'',
		#comment : ''"Merge commit 167cd135514a2855071c07c86f555bf3c697104f"\n'',
		#parentIds : [
			''7b3732dd9084205be9b22b57fd4cddbf39e9a004'',
			''167cd135514a2855071c07c86f555bf3c697104f''
		]
	},
	HiExampleCommit {
		#id : ''7b3732dd9084205be9b22b57fd4cddbf39e9a004'',
		#comment : ''Fixes #30, only one commit for several packages.\n'',
		#parentIds : [
			''61521823e500da3d236a574de880849bbed68536''
		]
	},
	HiExampleCommit {
		#id : ''167cd135514a2855071c07c86f555bf3c697104f'',
		#comment : ''Merge pull request #119 from jecisc/cf_ad_travis_configurations\n\nAdd travis configurations'',
		#parentIds : [
			''61521823e500da3d236a574de880849bbed68536'',
			''b53c84d42b41d3292594ebadd8c5cc671517483d''
		]
	},
	HiExampleCommit {
		#id : ''41cf141be4840154997fe687e2f35e9b67393f1d'',
		#comment : ''Typos'',
		#parentIds : [
			''61521823e500da3d236a574de880849bbed68536''
		]
	},
	HiExampleCommit {
		#id : ''61521823e500da3d236a574de880849bbed68536'',
		#comment : ''Allow to push on commit.\n'',
		#parentIds : [
			''c24d595dfee525afb662ac11646dea26232d5f3a''
		]
	},
	HiExampleCommit {
		#id : ''c24d595dfee525afb662ac11646dea26232d5f3a'',
		#comment : ''Fix commit message creation.\n'',
		#parentIds : [
			''6ea7f81c64233ae6aa5a492e8fdd9fddfc137ab3''
		]
	},
	HiExampleCommit {
		#id : ''6ea7f81c64233ae6aa5a492e8fdd9fddfc137ab3'',
		#comment : ''Change the implementation of commit browser, for supporting future commit customizations.\nChanged the glamour text for a spec model which is more powerful.\n'',
		#parentIds : [
			''63886ebeebabb7b921d31a13a1c0d695ee973df6''
		]
	},
	HiExampleCommit {
		#id : ''63886ebeebabb7b921d31a13a1c0d695ee973df6'',
		#comment : ''Fixes fast-forward merges were not loading the merged code into the image.\rCreate a very basic version of a history browser.\n'',
		#parentIds : [
			''32c00160d3d694a37365fd81ea53081a6481d54b''
		]
	},
	HiExampleCommit {
		#id : ''32c00160d3d694a37365fd81ea53081a6481d54b'',
		#comment : ''Fixes fast-forward merges were not loading the merged code into the image.\rCreate a very basic version of a history browser.\n'',
		#parentIds : [
			''4a5ecdf39d084fe054a2ed6531d9cbd320e3ccc9''
		]
	},
	HiExampleCommit {
		#id : ''4a5ecdf39d084fe054a2ed6531d9cbd320e3ccc9'',
		#comment : ''Merge branch \''master\'' of github.com:npasserini/iceberg\n'',
		#parentIds : [
			''296dfc974ef724dd30f1dd6cf62d4699e26c6f00'',
			''4757cfdb92b0a1bfeae0538613e1d1f54a016099''
		]
	},
	HiExampleCommit {
		#id : ''296dfc974ef724dd30f1dd6cf62d4699e26c6f00'',
		#comment : ''FIxes: #120 - Revert single change from the tree view.\n'',
		#parentIds : [
			''0f939ef16c7b53db33dde3676d0cf41e5fc0eb14''
		]
	},
	HiExampleCommit {
		#id : ''0f939ef16c7b53db33dde3676d0cf41e5fc0eb14'',
		#comment : ''Reorganize method.\n'',
		#parentIds : [
			''72673a9457ff877300d967177075f83ae800f335''
		]
	},
	HiExampleCommit {
		#id : ''72673a9457ff877300d967177075f83ae800f335'',
		#comment : ''Add an information message while committing.\n'',
		#parentIds : [
			''e5a84b97f148aae546ae38e42dd81ec5d486f79e''
		]
	},
	HiExampleCommit {
		#id : ''e5a84b97f148aae546ae38e42dd81ec5d486f79e'',
		#comment : ''New (still empty) browser for history.\n'',
		#parentIds : [
			''ac846c5c349c269afe583b5c5bb799188551961b''
		]
	},
	HiExampleCommit {
		#id : ''ac846c5c349c269afe583b5c5bb799188551961b'',
		#comment : ''New title.\n'',
		#parentIds : [
			''e1c8f7bb9514a2fa05b020da64ab30f3088409ab''
		]
	},
	HiExampleCommit {
		#id : ''4757cfdb92b0a1bfeae0538613e1d1f54a016099'',
		#comment : ''Fix\n'',
		#parentIds : [
			''e1c8f7bb9514a2fa05b020da64ab30f3088409ab''
		]
	},
	HiExampleCommit {
		#id : ''e1c8f7bb9514a2fa05b020da64ab30f3088409ab'',
		#comment : ''Back to original titles.\n'',
		#parentIds : [
			''d44acb4f4a2950d6b250a5d69d50a928a39f600b''
		]
	},
	HiExampleCommit {
		#id : ''d44acb4f4a2950d6b250a5d69d50a928a39f600b'',
		#comment : ''"Merge commit b08732aac922518f6ea613edd8214f53b7c5e0f8"\n'',
		#parentIds : [
			''ab2af4df3bce2f8bbb35488847d76f3c6e1d7326'',
			''b08732aac922518f6ea613edd8214f53b7c5e0f8''
		]
	},
	HiExampleCommit {
		#id : ''ab2af4df3bce2f8bbb35488847d76f3c6e1d7326'',
		#comment : ''Improved class documentation.\n'',
		#parentIds : [
			''e5837cf1436d462e323a39ac70c96b159d3c54f7''
		]
	},
	HiExampleCommit {
		#id : ''b08732aac922518f6ea613edd8214f53b7c5e0f8'',
		#comment : ''"Merging with e5837cf1436d462e323a39ac70c96b159d3c54f7"\n'',
		#parentIds : [
			''ba7a9de9819365f1ecec240fd3d0f9d009a26ab7'',
			''e5837cf1436d462e323a39ac70c96b159d3c54f7''
		]
	},
	HiExampleCommit {
		#id : ''ba7a9de9819365f1ecec240fd3d0f9d009a26ab7'',
		#comment : ''Darker title.\n'',
		#parentIds : [
			''c481019668a95f5d86c86f0b2eafa8b7bc1f43e4''
		]
	},
	HiExampleCommit {
		#id : ''e5837cf1436d462e323a39ac70c96b159d3c54f7'',
		#comment : ''New title.\n'',
		#parentIds : [
			''d83076b2d17d031fec316b4485f4d62ded78c7a3''
		]
	},
	HiExampleCommit {
		#id : ''d83076b2d17d031fec316b4485f4d62ded78c7a3'',
		#comment : ''Fix update presentation after merge.\n'',
		#parentIds : [
			''c481019668a95f5d86c86f0b2eafa8b7bc1f43e4''
		]
	},
	HiExampleCommit {
		#id : ''c481019668a95f5d86c86f0b2eafa8b7bc1f43e4'',
		#comment : ''Fix presentation update.\n'',
		#parentIds : [
			''b60d40d158ea9714e76eef06889d8df8a3fcf156''
		]
	},
	HiExampleCommit {
		#id : ''b60d40d158ea9714e76eef06889d8df8a3fcf156'',
		#comment : ''"Merging with 069c737a190ec9632db79b97cdd86891f0bc2a04"\n'',
		#parentIds : [
			''be0dbac1158de7b217db3497e9ddf5ddf98530d6'',
			''069c737a190ec9632db79b97cdd86891f0bc2a04''
		]
	},
	HiExampleCommit {
		#id : ''be0dbac1158de7b217db3497e9ddf5ddf98530d6'',
		#comment : ''Dark\n'',
		#parentIds : [
			''bba7735020ab29678b08cb0584491d882e465d29''
		]
	},
	HiExampleCommit {
		#id : ''069c737a190ec9632db79b97cdd86891f0bc2a04'',
		#comment : ''Lighter!\n'',
		#parentIds : [
			''bba7735020ab29678b08cb0584491d882e465d29''
		]
	},
	HiExampleCommit {
		#id : ''bba7735020ab29678b08cb0584491d882e465d29'',
		#comment : ''Upate browser after merging.\n'',
		#parentIds : [
			''e68cbdf935ae3c89078faa3818055fd32fe8eaf3''
		]
	},
	HiExampleCommit {
		#id : ''b53c84d42b41d3292594ebadd8c5cc671517483d'',
		#comment : ''Add travis configurations\n\n'',
		#parentIds : [
			''e68cbdf935ae3c89078faa3818055fd32fe8eaf3''
		]
	},
	HiExampleCommit {
		#id : ''e68cbdf935ae3c89078faa3818055fd32fe8eaf3'',
		#comment : ''"Merging with 96d646d8a2a639e633ad8543de70e016f1747eb1"\n'',
		#parentIds : [
			''d011f3fb91be46b76b52cd5a202e6d5138d31e7c'',
			''96d646d8a2a639e633ad8543de70e016f1747eb1''
		]
	},
	HiExampleCommit {
		#id : ''d011f3fb91be46b76b52cd5a202e6d5138d31e7c'',
		#comment : ''New status.\n'',
		#parentIds : [
			''68c01088adcc4d24851475ff2869760bef1bcb3c''
		]
	},
	HiExampleCommit {
		#id : ''96d646d8a2a639e633ad8543de70e016f1747eb1'',
		#comment : ''unsaved changes. => uncommited changes\n'',
		#parentIds : [
			''f5ccef7c727a347d7562fb2e342950f93204610b''
		]
	},
	HiExampleCommit {
		#id : ''68c01088adcc4d24851475ff2869760bef1bcb3c'',
		#comment : ''"Merge commit f5ccef7c727a347d7562fb2e342950f93204610b"\n'',
		#parentIds : [
			''fea94816cba6ce3114c513e389ba9a2ceda878d2'',
			''f5ccef7c727a347d7562fb2e342950f93204610b''
		]
	},
	HiExampleCommit {
		#id : ''fea94816cba6ce3114c513e389ba9a2ceda878d2'',
		#comment : ''outgoing => to be published\n'',
		#parentIds : [
			''2c12ee025ae90cc68f54aa2dd15837df5886f545''
		]
	},
	HiExampleCommit {
		#id : ''f5ccef7c727a347d7562fb2e342950f93204610b'',
		#comment : ''Uncommited changes => unsaved changes.\n'',
		#parentIds : [
			''ed446912fae5aaff68bb104efd129674363152d7''
		]
	},
	HiExampleCommit {
		#id : ''2c12ee025ae90cc68f54aa2dd15837df5886f545'',
		#comment : ''Add a new test for merging with a commit and refactor merging algorithm into several methods to make it simpler.\n'',
		#parentIds : [
			''db84e2ca4acd6a3f104866de1c90899c23f40da0''
		]
	},
	HiExampleCommit {
		#id : ''db84e2ca4acd6a3f104866de1c90899c23f40da0'',
		#comment : ''Add a new test for merging with a commit and refactor merging algorithm into several methods to make it simpler.\n'',
		#parentIds : [
			''61af8f3c7c883169255ebb9680e69b1b7c43be62''
		]
	},
	HiExampleCommit {
		#id : ''61af8f3c7c883169255ebb9680e69b1b7c43be62'',
		#comment : ''"Merge commit ed446912fae5aaff68bb104efd129674363152d7"\n'',
		#parentIds : [
			''74d455eec9f25d21ff1b497ae49712cc60191a03'',
			''ed446912fae5aaff68bb104efd129674363152d7''
		]
	},
	HiExampleCommit {
		#id : ''74d455eec9f25d21ff1b497ae49712cc60191a03'',
		#comment : ''Refactor the parsing of commit info which is done to create version histories. Ensure that each version knows its repository to avoid looking for parents in all known repositories in the world.\n'',
		#parentIds : [
			''dc6d32dc80071fb4aadce216f45d5ccd579fb7e1''
		]
	},
	HiExampleCommit {
		#id : ''dc6d32dc80071fb4aadce216f45d5ccd579fb7e1'',
		#comment : ''Change strategy for accumulating incomming commits due to SortedCollection bug https://github.com/npasserini/iceberg/issues/118\n'',
		#parentIds : [
			''a30c57af29d04b2f523efcb59ffb54272e677dac''
		]
	},
	HiExampleCommit {
		#id : ''a30c57af29d04b2f523efcb59ffb54272e677dac'',
		#comment : ''New title:  dark repositories\n'',
		#parentIds : [
			''c96033a4546064a3af2670591aa0dbc3aca03d34''
		]
	},
	HiExampleCommit {
		#id : ''ed446912fae5aaff68bb104efd129674363152d7'',
		#comment : ''New title: Light repositories\n'',
		#parentIds : [
			''c96033a4546064a3af2670591aa0dbc3aca03d34''
		]
	},
	HiExampleCommit {
		#id : ''c96033a4546064a3af2670591aa0dbc3aca03d34'',
		#comment : ''Change the way we compute incomming versions, better integrated with incomming commits. Clean unused methods.\n'',
		#parentIds : [
			''8122a7c635c5005969cc347c88643d9a6249e6fa''
		]
	},
	HiExampleCommit {
		#id : ''8122a7c635c5005969cc347c88643d9a6249e6fa'',
		#comment : ''"Merge commit be74be03f8a656f61af2a942742a8ea31db86d86"\n'',
		#parentIds : [
			''62562a5500416377749ccb0e4fed33257f1ddc45'',
			''be74be03f8a656f61af2a942742a8ea31db86d86''
		]
	},
	HiExampleCommit {
		#id : ''be74be03f8a656f61af2a942742a8ea31db86d86'',
		#comment : ''Repositories #7\n'',
		#parentIds : [
			''a9461f28541a6bba32e600c373c0ec327fd78c99''
		]
	},
	HiExampleCommit {
		#id : ''62562a5500416377749ccb0e4fed33257f1ddc45'',
		#comment : ''Refresh to reload versions before updating working copy ancestry.\n'',
		#parentIds : [
			''84a7c4cf48c6284aefdf6a87c8a945f5237cc654''
		]
	},
	HiExampleCommit {
		#id : ''84a7c4cf48c6284aefdf6a87c8a945f5237cc654'',
		#comment : ''"Merging with a9461f28541a6bba32e600c373c0ec327fd78c99"\n'',
		#parentIds : [
			''1086aba033b28877ec229ccb78bd9116b4a0c491'',
			''a9461f28541a6bba32e600c373c0ec327fd78c99''
		]
	},
	HiExampleCommit {
		#id : ''a9461f28541a6bba32e600c373c0ec327fd78c99'',
		#comment : ''Repositories #6\n'',
		#parentIds : [
			''5bb825e61c7d84c92ef0c03825d674dfc45718ae''
		]
	},
	HiExampleCommit {
		#id : ''1086aba033b28877ec229ccb78bd9116b4a0c491'',
		#comment : ''Remove halt and change title for testing\n'',
		#parentIds : [
			''a603957f8f5d03818e4732aff9d7307c7eb3c3c4''
		]
	},
	HiExampleCommit {
		#id : ''a603957f8f5d03818e4732aff9d7307c7eb3c3c4'',
		#comment : ''"Merge commit 5bb825e61c7d84c92ef0c03825d674dfc45718ae"\n'',
		#parentIds : [
			''8717508a176819be996615f032874528d60c45b0'',
			''5bb825e61c7d84c92ef0c03825d674dfc45718ae''
		]
	},
	HiExampleCommit {
		#id : ''8717508a176819be996615f032874528d60c45b0'',
		#comment : ''Fix fetch\n'',
		#parentIds : [
			''58ecc42ead3c9e051d13774becdefaa18efba53e''
		]
	},
	HiExampleCommit {
		#id : ''58ecc42ead3c9e051d13774becdefaa18efba53e'',
		#comment : ''Update working copy ancestry after a merge commit.\n'',
		#parentIds : [
			''14a46920aa906a6d46deb66c748cfeee260f2f6b''
		]
	},
	HiExampleCommit {
		#id : ''5bb825e61c7d84c92ef0c03825d674dfc45718ae'',
		#comment : ''Repositories #5\n'',
		#parentIds : [
			''f433c5700efbedb6604af4cb75af072d5edf185f''
		]
	},
	HiExampleCommit {
		#id : ''14a46920aa906a6d46deb66c748cfeee260f2f6b'',
		#comment : ''"Merging with 808dff6b5d9540bcd7d78debfe2962648d9e97a2"\n'',
		#parentIds : [
			''d925f44c48718240d661787d93234ba76d333be4'',
			''808dff6b5d9540bcd7d78debfe2962648d9e97a2''
		]
	},
	HiExampleCommit {
		#id : ''d925f44c48718240d661787d93234ba76d333be4'',
		#comment : ''New merge strategy.\n'',
		#parentIds : [
			''902caad0a9cc6ab90576d5fe39cc6e9f27b2f8a1''
		]
	},
	HiExampleCommit {
		#id : ''902caad0a9cc6ab90576d5fe39cc6e9f27b2f8a1'',
		#comment : ''New merge strategy.\n'',
		#parentIds : [
			''acb86a4d43cd08a6f683607c19e2fefe1d562497''
		]
	},
	HiExampleCommit {
		#id : ''acb86a4d43cd08a6f683607c19e2fefe1d562497'',
		#comment : ''Small performance optimisation when looking for commit ancestors.\n'',
		#parentIds : [
			''f730652629bc4d302ef5776aa33f94891c7eca8c''
		]
	},
	HiExampleCommit {
		#id : ''f730652629bc4d302ef5776aa33f94891c7eca8c'',
		#comment : ''Title #5\n'',
		#parentIds : [
			''4d737ea67589c2eee2c12d7657cfe17cb41e2cb8''
		]
	},
	HiExampleCommit {
		#id : ''f433c5700efbedb6604af4cb75af072d5edf185f'',
		#comment : ''Repositories #4\n'',
		#parentIds : [
			''808dff6b5d9540bcd7d78debfe2962648d9e97a2''
		]
	},
	HiExampleCommit {
		#id : ''808dff6b5d9540bcd7d78debfe2962648d9e97a2'',
		#comment : ''Repositories #3\n'',
		#parentIds : [
			''ec6425d25fd83f627554f4195d4434118aa9486a''
		]
	},
	HiExampleCommit {
		#id : ''4d737ea67589c2eee2c12d7657cfe17cb41e2cb8'',
		#comment : ''"Merge commit ec6425d25fd83f627554f4195d4434118aa9486a"\n'',
		#parentIds : [
			''d04dd4f3de6412114f4de8e18d8b274999681530'',
			''ec6425d25fd83f627554f4195d4434118aa9486a''
		]
	},
	HiExampleCommit {
		#id : ''d04dd4f3de6412114f4de8e18d8b274999681530'',
		#comment : ''Improved merge commit message.\n'',
		#parentIds : [
			''b359f3bafbb3dbc9ead657f74a97c87f82d928ea''
		]
	},
	HiExampleCommit {
		#id : ''ec6425d25fd83f627554f4195d4434118aa9486a'',
		#comment : ''My new title\n'',
		#parentIds : [
			''310a775c3209e0511ad0c6f86dbc67b66bcdf599''
		]
	},
	HiExampleCommit {
		#id : ''b359f3bafbb3dbc9ead657f74a97c87f82d928ea'',
		#comment : ''"Merge commit 2"\n'',
		#parentIds : [
			''0de525cca0c8d06d6e118f5bca71b7460bff3b1f'',
			''203a6c7429d694de219c3ca8678adf9789fb1fda''
		]
	},
	HiExampleCommit {
		#id : ''0de525cca0c8d06d6e118f5bca71b7460bff3b1f'',
		#comment : ''Refresh after fetching. Refresh repository together with diff refreshing.\n'',
		#parentIds : [
			''310a775c3209e0511ad0c6f86dbc67b66bcdf599''
		]
	},
	HiExampleCommit {
		#id : ''203a6c7429d694de219c3ca8678adf9789fb1fda'',
		#comment : ''Change title.\n'',
		#parentIds : [
			''310a775c3209e0511ad0c6f86dbc67b66bcdf599''
		]
	},
	HiExampleCommit {
		#id : ''310a775c3209e0511ad0c6f86dbc67b66bcdf599'',
		#comment : '' - Fixed Adding package to a repository\r - next version name should work for saved and non-saved packages\n'',
		#parentIds : [
			''53fa3562ccd69bf739812b03873477892706ce46''
		]
	},
	HiExampleCommit {
		#id : ''53fa3562ccd69bf739812b03873477892706ce46'',
		#comment : ''Fixes #111 - Iceberg does not parse URLs containing dashes in usernames\n'',
		#parentIds : [
			''6c33489921654fe2e3d9bd6673d0231a12dfdca4''
		]
	},
	HiExampleCommit {
		#id : ''6c33489921654fe2e3d9bd6673d0231a12dfdca4'',
		#comment : ''Fix bugs found after refactoring commit strategy.\n'',
		#parentIds : [
			''874afcb43e71b0763d4fd9a398a5766395095b89''
		]
	},
	HiExampleCommit {
		#id : ''874afcb43e71b0763d4fd9a398a5766395095b89'',
		#comment : ''Merge remote-tracking branch \''origin/master\''\n'',
		#parentIds : [
			''b0efae6374db48f815b36b119cca8ee71c228092'',
			''7e3dd276695446899011ca877c239d7531692e41''
		]
	},
	HiExampleCommit {
		#id : ''b0efae6374db48f815b36b119cca8ee71c228092'',
		#comment : ''Fix bugs found after refactoring commit strategy.\n'',
		#parentIds : [
			''b5626abaf4d0320bf6a5f8061d7f1faf9c8f9df1''
		]
	},
	HiExampleCommit {
		#id : ''b5626abaf4d0320bf6a5f8061d7f1faf9c8f9df1'',
		#comment : ''Refactor committing strategy, support for saving multiple packages in one commit and manual merging.\n'',
		#parentIds : [
			''5c4dee1beb09e70f20bc682044a59f4241603e74''
		]
	},
	HiExampleCommit {
		#id : ''5c4dee1beb09e70f20bc682044a59f4241603e74'',
		#comment : ''Abort merge if conflicts arise.\n'',
		#parentIds : [
			''faf606166db96f1a4a125dceedae67922ba049dd''
		]
	},
	HiExampleCommit {
		#id : ''faf606166db96f1a4a125dceedae67922ba049dd'',
		#comment : ''Test improvements.\n'',
		#parentIds : [
			''757432185ebb90cbe23e103075c9cb2c139af82f''
		]
	},
	HiExampleCommit {
		#id : ''757432185ebb90cbe23e103075c9cb2c139af82f'',
		#comment : ''Change diff computing for incomming commits, compare commit vs. merge base instead of comparing vs. working copy. This allows to see the changes introduc\n(END)\n'',
		#parentIds : [
			''7249adf52e5848f76db07a6090b83b9c8b86c456''
		]
	},
	HiExampleCommit {
		#id : ''7249adf52e5848f76db07a6090b83b9c8b86c456'',
		#comment : ''Refactor to allow for showing changes from incomming commits, have a single commit registry for each repository, which holds repositories for all branches.\n'',
		#parentIds : [
			''e6ca21def18a93766ec0e258c2f3575145dac204''
		]
	},
	HiExampleCommit {
		#id : ''e6ca21def18a93766ec0e258c2f3575145dac204'',
		#comment : ''Refactor to allow for showing changes from incomming commits, have a single commit registry for each repository, which holds repositories for all branches.\n'',
		#parentIds : [
			''1bac0c81e61b3344ee826273fecd401cc141d762''
		]
	},
	HiExampleCommit {
		#id : ''7e3dd276695446899011ca877c239d7531692e41'',
		#comment : ''Merge pull request #109 from peteruhnak/master\n\nFixes #95 - Use #chooseDirectoryFrom:  to preselect the location, and not pick the title.'',
		#parentIds : [
			''13947f252018a12a5242d3124150fb0e35c26b38'',
			''cb4cb5b0bd401c28630aaeaa853b2f0e6c21e72b''
		]
	},
	HiExampleCommit {
		#id : ''1bac0c81e61b3344ee826273fecd401cc141d762'',
		#comment : ''Better importing of existing repositories.\n'',
		#parentIds : [
			''4f8e140af299d6b12fdd90dde880b341e73d466d''
		]
	},
	HiExampleCommit {
		#id : ''4f8e140af299d6b12fdd90dde880b341e73d466d'',
		#comment : ''Forbid to create two repositories in the same location (local directory).\nReorganized errors.\nImproved error messages.\n'',
		#parentIds : [
			''7fb664ec0bb3ac91807c9e095af4bff60d0a7f93''
		]
	},
	HiExampleCommit {
		#id : ''7fb664ec0bb3ac91807c9e095af4bff60d0a7f93'',
		#comment : ''Forbid to create two repositories in the same location (local directory).\nReorganized errors.\nImproved error messages.\n'',
		#parentIds : [
			''166ad5f45af0de23459ca7c15a07bca0efd70d36''
		]
	},
	HiExampleCommit {
		#id : ''166ad5f45af0de23459ca7c15a07bca0efd70d36'',
		#comment : ''Refresh view after unloading package.\n'',
		#parentIds : [
			''13947f252018a12a5242d3124150fb0e35c26b38''
		]
	},
	HiExampleCommit {
		#id : ''cb4cb5b0bd401c28630aaeaa853b2f0e6c21e72b'',
		#comment : ''Fixes #95 - Use #chooseDirectoryFrom:  to preselect the location, and not pick the title.\n'',
		#parentIds : [
			''13947f252018a12a5242d3124150fb0e35c26b38''
		]
	},
	HiExampleCommit {
		#id : ''13947f252018a12a5242d3124150fb0e35c26b38'',
		#comment : ''Fixes: #108 - When double clicking on a repository it opens multiple synchronization windows\n'',
		#parentIds : [
			''8676a9f6624eb64a929da36cd6d165ad6d536065''
		]
	},
	HiExampleCommit {
		#id : ''8676a9f6624eb64a929da36cd6d165ad6d536065'',
		#comment : ''Fixes #58 - Registry should produce an anouncement when creating a repository.\rRenamed IceCreateRepositoryModel to IceCloneRepositoryModel.\rAllow for git SCP urls without a trailing \''.git\''.\rSlightly improving of error messages on wrong repository URLs.\n'',
		#parentIds : [
			''359fae998280eb39c1c89fb9757790ef081708fd''
		]
	},
	HiExampleCommit {
		#id : ''359fae998280eb39c1c89fb9757790ef081708fd'',
		#comment : ''Fixes #104 - Add repository info.\n'',
		#parentIds : [
			''8eba559423ce5b10f0532d6ae529b591959c4926''
		]
	},
	HiExampleCommit {
		#id : ''8eba559423ce5b10f0532d6ae529b591959c4926'',
		#comment : ''Fixes #102, correctly show incomming commits (from upstream).\n'',
		#parentIds : [
			''82f1ed65467ab40e041efd731d79d7f19b472d4e''
		]
	},
	HiExampleCommit {
		#id : ''82f1ed65467ab40e041efd731d79d7f19b472d4e'',
		#comment : ''Fixes #102, correctly show incomming commits (from upstream).\n'',
		#parentIds : [
			''b6f50b2e26e4fcc8fef58197a271a99a395addde''
		]
	},
	HiExampleCommit {
		#id : ''b6f50b2e26e4fcc8fef58197a271a99a395addde'',
		#comment : ''Fix UI update after push.\n'',
		#parentIds : [
			''03dcf38d6177ae6d5566b6f7d44b929f3276235f''
		]
	},
	HiExampleCommit {
		#id : ''03dcf38d6177ae6d5566b6f7d44b929f3276235f'',
		#comment : ''Add push from the main window\n'',
		#parentIds : [
			''993814834677daa305e9bddad3e7ffa963c2a854''
		]
	},
	HiExampleCommit {
		#id : ''993814834677daa305e9bddad3e7ffa963c2a854'',
		#comment : ''Add push from the main window\n'',
		#parentIds : [
			''c2cca72175c491207a87845f5b261868f30165ea''
		]
	},
	HiExampleCommit {
		#id : ''c2cca72175c491207a87845f5b261868f30165ea'',
		#comment : ''Fixes #98 - When creating an IceRepository out of an MCGithubRepository, keep the same branch.\n'',
		#parentIds : [
			''cd95e693e00fbd0387ae84e11e6a80a4a8be743d''
		]
	},
	HiExampleCommit {
		#id : ''cd95e693e00fbd0387ae84e11e6a80a4a8be743d'',
		#comment : ''Fixes #98 - When creating an IceRepository out of an MCGithubRepository, keep the same branch.\n'',
		#parentIds : [
			''191f83fa0ada44f58e2ff5efc7435b85d5489e51''
		]
	},
	HiExampleCommit {
		#id : ''191f83fa0ada44f58e2ff5efc7435b85d5489e51'',
		#comment : ''Pull and merge\n'',
		#parentIds : [
			''4876058b64f1189d273ba42ca28e0c55767f8b3b''
		]
	},
	HiExampleCommit {
		#id : ''4876058b64f1189d273ba42ca28e0c55767f8b3b'',
		#comment : ''Pull and merge\n'',
		#parentIds : [
			''d1de18863e3a01bf26a70938a24e35e8dbaeaebb''
		]
	},
	HiExampleCommit {
		#id : ''d1de18863e3a01bf26a70938a24e35e8dbaeaebb'',
		#comment : ''Added pull\n'',
		#parentIds : [
			''fa82bce7f8aa49a1dcb88a734253db43144e542b''
		]
	},
	HiExampleCommit {
		#id : ''fa82bce7f8aa49a1dcb88a734253db43144e542b'',
		#comment : ''Libgit adapter should not return lgit objects, just return branch name\n'',
		#parentIds : [
			''daee2b4d07e419958ccdae786a9b86735e589b34''
		]
	},
	HiExampleCommit {
		#id : ''daee2b4d07e419958ccdae786a9b86735e589b34'',
		#comment : ''Refactor to avoid using --abbrev-ref in rev-parse, which is not supported by libgit2.\n'',
		#parentIds : [
			''ff772308f63d8fa834a7d28c87d6480b5735d981''
		]
	},
	HiExampleCommit {
		#id : ''ff772308f63d8fa834a7d28c87d6480b5735d981'',
		#comment : ''Refactor to avoid using --abbrev-ref in rev-parse, which is not supported by libgit2.\n'',
		#parentIds : [
			''adf67f3a80b3a009f8e8783627bc41ba76c46af7''
		]
	},
	HiExampleCommit {
		#id : ''adf67f3a80b3a009f8e8783627bc41ba76c46af7'',
		#comment : ''Creating a good merge is still difficult, we rollback for a while.\n'',
		#parentIds : [
			''ef455ce8078e5d472d07e3e97c42afd47b3c434b''
		]
	},
	HiExampleCommit {
		#id : ''ef455ce8078e5d472d07e3e97c42afd47b3c434b'',
		#comment : ''"Merge commit 2"\n'',
		#parentIds : [
			''60a585e7d916a0b8612ee492e73053bac00e5680'',
			''2ddc815b60f7d4a6c7532a447e7a9f55bb71f37f''
		]
	},
	HiExampleCommit {
		#id : ''60a585e7d916a0b8612ee492e73053bac00e5680'',
		#comment : ''Invoke merge tool when \''merge  commit\'' action is selected.\n'',
		#parentIds : [
			''7dbdb069cdc09799cf5699ac8ff5e8d40a07f36a''
		]
	},
	HiExampleCommit {
		#id : ''7dbdb069cdc09799cf5699ac8ff5e8d40a07f36a'',
		#comment : ''Fix IceBranch creation as it in some points ended having a reference to a wrong repository.\n'',
		#parentIds : [
			''98de42ad2951f251bda34669b48687880eadbf8a''
		]
	},
	HiExampleCommit {
		#id : ''98de42ad2951f251bda34669b48687880eadbf8a'',
		#comment : ''Automatically compute upstream branch or a repository.\n'',
		#parentIds : [
			''66432581cbeb6552a66645460f2ca14b805205e3''
		]
	},
	HiExampleCommit {
		#id : ''66432581cbeb6552a66645460f2ca14b805205e3'',
		#comment : ''Automatically compute upstream branch or a repository.\n'',
		#parentIds : [
			''67238198bb7d58b24c973c07e64efa885031b9f6''
		]
	},
	HiExampleCommit {
		#id : ''67238198bb7d58b24c973c07e64efa885031b9f6'',
		#comment : ''Refactor for allowing a repository to have more than one  \''commit history\'', i.e. one associate to the current branch (or HEAD) and one to the configured upstream (for example \''origin/master\''). In this way, the repository can handle information about incomming versions and commits (from upstream) in the same way that they are handled in the current branch, but without affecting the HEAD or working copy.\r\rTo do so, several responsibilities that were in repository have been moved to the branch object.\n'',
		#parentIds : [
			''250276e729e57dfcae337763e2f615bedd906d8b''
		]
	},
	HiExampleCommit {
		#id : ''250276e729e57dfcae337763e2f615bedd906d8b'',
		#comment : ''Refactor for allowing a repository to have more than one  \''commit history\'', i.e. one associate to the current branch (or HEAD) and one to the configured upstream (for example \''origin/master\''). In this way, the repository can handle information about incomming versions and commits (from upstream) in the same way that they are handled in the current branch, but without affecting the HEAD or working copy.\r\rTo do so, several responsibilities that were in repository have been moved to the branch object.\n'',
		#parentIds : [
			''b3a7576e99fade33896c2fdf5b9cd2f29cf23b46''
		]
	},
	HiExampleCommit {
		#id : ''2ddc815b60f7d4a6c7532a447e7a9f55bb71f37f'',
		#comment : ''Fix update comment\n'',
		#parentIds : [
			''b3a7576e99fade33896c2fdf5b9cd2f29cf23b46''
		]
	},
	HiExampleCommit {
		#id : ''b3a7576e99fade33896c2fdf5b9cd2f29cf23b46'',
		#comment : ''Add more branches to the GUI checkout options, now it shows all local branches and all remote branches that have not been checked out locally.\n'',
		#parentIds : [
			''2e6bac0bbda3349b02c072ba7ab4e0a1b973b76b''
		]
	},
	HiExampleCommit {
		#id : ''2e6bac0bbda3349b02c072ba7ab4e0a1b973b76b'',
		#comment : ''Fix bug that prevented from correctly deleting test packages and makes some tests fail not deterministically.\n'',
		#parentIds : [
			''b3e08144aa22ad5b9c24a1ca1eb38e819f036057''
		]
	},
	HiExampleCommit {
		#id : ''b3e08144aa22ad5b9c24a1ca1eb38e819f036057'',
		#comment : ''Remove debugging code.\n'',
		#parentIds : [
			''f3f1faa289972181e5242279a2bc566bd2b61d0f''
		]
	},
	HiExampleCommit {
		#id : ''f3f1faa289972181e5242279a2bc566bd2b61d0f'',
		#comment : ''Refactor how to decide is one version/commit is ancestor of another one.\n'',
		#parentIds : [
			''4ff04bad91d5a97ab84e96b072eb8d24d28e9f11''
		]
	},
	HiExampleCommit {
		#id : ''4ff04bad91d5a97ab84e96b072eb8d24d28e9f11'',
		#comment : ''Refactor how to decide is one version/commit is ancestor of another one.\n'',
		#parentIds : [
			''7853c75d39c13f80907b4e287467bdf5418537f1''
		]
	},
	HiExampleCommit {
		#id : ''7853c75d39c13f80907b4e287467bdf5418537f1'',
		#comment : ''Fixes #97: Allow to forget repositories from the GUI\n'',
		#parentIds : [
			''15943debfe03d48002d5af70b0561fffee38e5d5''
		]
	},
	HiExampleCommit {
		#id : ''15943debfe03d48002d5af70b0561fffee38e5d5'',
		#comment : ''Merge branch \''master\'' of github.com:npasserini/iceberg\n'',
		#parentIds : [
			''c3e415c13d541416f00dc41a3dbc65473952f3f5'',
			''6518b3ece81dcde31623a63498a3c5964bd2af8d''
		]
	},
	HiExampleCommit {
		#id : ''c3e415c13d541416f00dc41a3dbc65473952f3f5'',
		#comment : ''Avoid using command: from the GUI.\n'',
		#parentIds : [
			''ee254eaf286f51f5abc16b890627ff0f8166dc13''
		]
	},
	HiExampleCommit {
		#id : ''ee254eaf286f51f5abc16b890627ff0f8166dc13'',
		#comment : ''Fix #testLoadCommit\n'',
		#parentIds : [
			''d6418748d4253b1e99ad6c8be15ee08540ece607''
		]
	},
	HiExampleCommit {
		#id : ''6518b3ece81dcde31623a63498a3c5964bd2af8d'',
		#comment : ''Fixes #93 - Prevent code changes done by tests to be shown in epicea sessions\n'',
		#parentIds : [
			''d6418748d4253b1e99ad6c8be15ee08540ece607''
		]
	},
	HiExampleCommit {
		#id : ''d6418748d4253b1e99ad6c8be15ee08540ece607'',
		#comment : ''Allow to create commits and push.\rCreate extensions of monticello and cypress writers, to write directory to a git blob through libgit2.\rIceLibgitTreeBuilder allows for building a new tree with the new commit.\n'',
		#parentIds : [
			''0c4f221875373ad0e722e1beae62eed7559c29dc''
		]
	},
	HiExampleCommit {
		#id : ''0c4f221875373ad0e722e1beae62eed7559c29dc'',
		#comment : ''Fixes #86 (create a version info from git commits in blob) and also allows to nicely load packages from git blob.\rThis allows for several cleanups in IceSavedPackageVersion.\rI added a little hack to allow for git tree entries to be polymorphic with commits.\n'',
		#parentIds : [
			''5e9cd1270ff4a16a129faa923a25cafcc94613d3''
		]
	},
	HiExampleCommit {
		#id : ''5e9cd1270ff4a16a129faa923a25cafcc94613d3'',
		#comment : ''Loading code from libgit repository.\n'',
		#parentIds : [
			''4a26963255c48b772ffca58936e9a920d7191b58''
		]
	},
	HiExampleCommit {
		#id : ''4a26963255c48b772ffca58936e9a920d7191b58'',
		#comment : ''Read classes and methods from a git blob. Create classes to allow for polymorphism between a git blob and normal filetree directory readers.\n'',
		#parentIds : [
			''ba4a627ac87a7cf2c475a412d17cef8b3ec0c0e2''
		]
	},
	HiExampleCommit {
		#id : ''ba4a627ac87a7cf2c475a412d17cef8b3ec0c0e2'',
		#comment : ''Read classes and methods from a git blob. Create classes to allow for polymorphism between a git blob and normal filetree directory readers.\n'',
		#parentIds : [
			''0f5fab21b9c360293ddcd350919c86c1269a3b7a''
		]
	},
	HiExampleCommit {
		#id : ''0f5fab21b9c360293ddcd350919c86c1269a3b7a'',
		#comment : ''Fixed test create branch.\n'',
		#parentIds : [
			''0a6c59c2953ab26267c030829a0933e65ddb1ee6''
		]
	},
	HiExampleCommit {
		#id : ''0a6c59c2953ab26267c030829a0933e65ddb1ee6'',
		#comment : ''Fix bug in new push implementation.\n'',
		#parentIds : [
			''930e48c5cf0546a02e35f1c98fce5751b79baa99''
		]
	},
	HiExampleCommit {
		#id : ''930e48c5cf0546a02e35f1c98fce5751b79baa99'',
		#comment : ''Changed baseline to allow for new branch of libgit, for pharo60.\rExtended Iceberg update to take care of libgit.\rFix = of IceCommitInfo (do not break if compared to nil).\rFixed createBranch; delegate to backend; improved test (fixes: 84)\rDelegate push and localBranchNames to backend repository.\routgoingCommits returns allCommits if there is no associated upstream.\n'',
		#parentIds : [
			''04b86176c8bf18855b8c9f96d2fa544c602cb644''
		]
	},
	HiExampleCommit {
		#id : ''04b86176c8bf18855b8c9f96d2fa544c602cb644'',
		#comment : ''Changed baseline to allow for new branch of libgit, for pharo60.\rExtended Iceberg update to take care of libgit.\rFix = of IceCommitInfo (do not break if compared to nil).\rFixed createBranch; delegate to backend; improved test (fixes: 84)\rDelegate push and localBranchNames to backend repository.\routgoingCommits returns allCommits if there is no associated upstream.\n'',
		#parentIds : [
			''ed1170ada2393733be91319d1f7ecdee49686a7b''
		]
	},
	HiExampleCommit {
		#id : ''ed1170ada2393733be91319d1f7ecdee49686a7b'',
		#comment : ''Changed baseline to allow for new branch of libgit, for pharo60.\rExtended Iceberg update to take care of libgit.\rFix = of IceCommitInfo (do not break if compared to nil).\rFixed createBranch; delegate to backend; improved test (fixes: 84)\rDelegate push and localBranchNames to backend repository.\routgoingCommits returns allCommits if there is no associated upstream.\n'',
		#parentIds : [
			''b9f202a376c540c3d41c5d8134ab0abf6e198fd5''
		]
	},
	HiExampleCommit {
		#id : ''b9f202a376c540c3d41c5d8134ab0abf6e198fd5'',
		#comment : ''Multiple refactors, delegating from IceRepository to its backend and to SavedPackage, in order to allow for supporting libgit.\n'',
		#parentIds : [
			''ae820511e92ccc08a6118d8d05698f2d223b901c''
		]
	},
	HiExampleCommit {
		#id : ''ae820511e92ccc08a6118d8d05698f2d223b901c'',
		#comment : ''Multiple refactors, delegating from IceRepository to its backend and to SavedPackage, in order to allow for supporting libgit.\n'',
		#parentIds : [
			''144b88433a9db9d902142e4f289c6c4245bb7c05''
		]
	},
	HiExampleCommit {
		#id : ''144b88433a9db9d902142e4f289c6c4245bb7c05'',
		#comment : ''Delegate lookup of packages to backend repositories. Create Trait for avoiding duplicated behavior between backends.\n'',
		#parentIds : [
			''2364544397e3e2ed7080762a7fd519658721f450''
		]
	},
	HiExampleCommit {
		#id : ''2364544397e3e2ed7080762a7fd519658721f450'',
		#comment : ''Delegate lookup of packages to backend repositories. Create Trait for avoiding duplicated behavior between backends.\n'',
		#parentIds : [
			''9a21de7bee1d0facfa4897e9636ceb586899dd65''
		]
	},
	HiExampleCommit {
		#id : ''9a21de7bee1d0facfa4897e9636ceb586899dd65'',
		#comment : ''Fixes #79, checkout branch through libgit.\rAlso refactor some repeated code into withRepoDo:\n'',
		#parentIds : [
			''943751f93aa41f5cac7c405f0922ee6810927bcb''
		]
	},
	HiExampleCommit {
		#id : ''943751f93aa41f5cac7c405f0922ee6810927bcb'',
		#comment : ''Fixes #78, obtain current branch through libgit.\n'',
		#parentIds : [
			''86d34468f5777f1f047ef862dbfcd70b9305e72c''
		]
	},
	HiExampleCommit {
		#id : ''86d34468f5777f1f047ef862dbfcd70b9305e72c'',
		#comment : ''Fixes #78, obtain current branch through libgit.\n'',
		#parentIds : [
			''bd720257091217d5f0cdb97a3c4cd65a2b43e39f''
		]
	},
	HiExampleCommit {
		#id : ''bd720257091217d5f0cdb97a3c4cd65a2b43e39f'',
		#comment : ''Ensure to remove repositories from registry after running tests.\n'',
		#parentIds : [
			''19e33f31d14ae68f302230ffa50f150e3ead9819''
		]
	},
	HiExampleCommit {
		#id : ''19e33f31d14ae68f302230ffa50f150e3ead9819'',
		#comment : ''Ensure to remove repositories from registry after running tests.\n'',
		#parentIds : [
			''fae4fbb8b4c33c876ff8897953cb6bec692a5aee''
		]
	},
	HiExampleCommit {
		#id : ''fae4fbb8b4c33c876ff8897953cb6bec692a5aee'',
		#comment : ''Fix tests to run with local repository. All green tests again.\n'',
		#parentIds : [
			''090f292d305561c912b9943ac0c3da839b1142ee''
		]
	},
	HiExampleCommit {
		#id : ''090f292d305561c912b9943ac0c3da839b1142ee'',
		#comment : ''Fix tests to run with local repository. All green tests again.\n'',
		#parentIds : [
			''068bf6297d24d05432462ccf259ef442b4d91188''
		]
	},
	HiExampleCommit {
		#id : ''068bf6297d24d05432462ccf259ef442b4d91188'',
		#comment : ''Refactor IceRemoteRepository to make place for file:// repos.\n'',
		#parentIds : [
			''89260d2abfd12cc652d876802d92b6ddcd727020''
		]
	},
	HiExampleCommit {
		#id : ''89260d2abfd12cc652d876802d92b6ddcd727020'',
		#comment : ''Refactor IceRemoteRepository to make place for file:// repos.\n'',
		#parentIds : [
			''642b6b0bde05c48b3c22263694b9854d35233dfb''
		]
	},
	HiExampleCommit {
		#id : ''642b6b0bde05c48b3c22263694b9854d35233dfb'',
		#comment : ''Libgit cloning, but using https.\rAlso, fixed bare repositories creation, in order to handle them a remote repo instead of a url string.\n'',
		#parentIds : [
			''80f36e5d9dfc41a062bae52c2207bcd564145409''
		]
	},
	HiExampleCommit {
		#id : ''80f36e5d9dfc41a062bae52c2207bcd564145409'',
		#comment : ''Fixes #76 - Refactor IceMCGitRepository to remove the localRepository variable, which can be computed.\rAlso delegate some of the creation to the repository itself, since it will be different in the new one with libgit.\n'',
		#parentIds : [
			''9ca99cfcb3a76fc616e225960d2fe7df5ab802ce''
		]
	},
	HiExampleCommit {
		#id : ''9ca99cfcb3a76fc616e225960d2fe7df5ab802ce'',
		#comment : ''Delegate clone to the bare repository\n'',
		#parentIds : [
			''8863137f81839631bd15111d9f4295987a70343b''
		]
	},
	HiExampleCommit {
		#id : ''8863137f81839631bd15111d9f4295987a70343b'',
		#comment : ''Delegate command to the bare repository\n'',
		#parentIds : [
			''807b82407e08702f7da3fcfe2edec0c4df6a6b5f''
		]
	},
	HiExampleCommit {
		#id : ''807b82407e08702f7da3fcfe2edec0c4df6a6b5f'',
		#comment : ''Add package group developement to baseline.\n'',
		#parentIds : [
			''afb6e13d2896b634b1ff548cc6cf56a87d5a9ccd''
		]
	},
	HiExampleCommit {
		#id : ''afb6e13d2896b634b1ff548cc6cf56a87d5a9ccd'',
		#comment : ''Change branch of filetree.\n'',
		#parentIds : [
			''cd40a36325b959ab77b1ce03e44c48ff0e6afe97''
		]
	},
	HiExampleCommit {
		#id : ''cd40a36325b959ab77b1ce03e44c48ff0e6afe97'',
		#comment : ''Include libgit as dependency.\n'',
		#parentIds : [
			''c08d5df92afad84b5e779a3e472f6bb1aadc11d6''
		]
	},
	HiExampleCommit {
		#id : ''c08d5df92afad84b5e779a3e472f6bb1aadc11d6'',
		#comment : ''Prepare IceRepository to have two alternative storages (gitfiletree and libgit)\n'',
		#parentIds : [
			''c0f9b048875e380fd358aa7c684f5d300604efb8''
		]
	},
	HiExampleCommit {
		#id : ''c0f9b048875e380fd358aa7c684f5d300604efb8'',
		#comment : ''Prepare IceRepository to have two alternative storages (gitfiletree and libgit)\n'',
		#parentIds : [
			''f626b3ea61e4cbd2850a064306f50d8dee93e7da''
		]
	},
	HiExampleCommit {
		#id : ''f626b3ea61e4cbd2850a064306f50d8dee93e7da'',
		#comment : ''Improved docs for v0.1\n'',
		#parentIds : [
			''1603f8cd6d1f7ef0dddd2943bc63a08fc5355f6c''
		]
	},
	HiExampleCommit {
		#id : ''1603f8cd6d1f7ef0dddd2943bc63a08fc5355f6c'',
		#comment : ''Fixes #67 - Lookup for versions of a package in parents of a commit if the commit has no changes for the specified package.\rKeep ancestry of git commits in the IceCommitInfo object.\rChange the cache of git commits to be a map indexed by the commitId\rImprove printing of IceSavedPackage\rFix and improve tests.\n'',
		#parentIds : [
			''3f1ad7b3a7292bdebbcd27179611ffd2b7f0acc3''
		]
	},
	HiExampleCommit {
		#id : ''3f1ad7b3a7292bdebbcd27179611ffd2b7f0acc3'',
		#comment : ''Fixes #67 - Lookup for versions of a package in parents of a commit if the commit has no changes for the specified package.\rKeep ancestry of git commits in the IceCommitInfo object.\rChange the cache of git commits to be a map indexed by the commitId\rImprove printing of IceSavedPackage\rFix and improve tests.\n'',
		#parentIds : [
			''791f9729cf767c9367ff871a2ee6a5ea484ce880''
		]
	},
	HiExampleCommit {
		#id : ''791f9729cf767c9367ff871a2ee6a5ea484ce880'',
		#comment : ''Merge branch \''master\'' of github.com:npasserini/iceberg\n'',
		#parentIds : [
			''8c57f475d7d99244ba38d607c86a37660864da3a'',
			''0a498c8884b1b753135e5d33228a86e191d37f80''
		]
	},
	HiExampleCommit {
		#id : ''8c57f475d7d99244ba38d607c86a37660864da3a'',
		#comment : ''Fixes #50 - GUI for adding existing package to an IceRepository\n'',
		#parentIds : [
			''3fa8f6af583d6bacf6ed801e47494c74279151de''
		]
	},
	HiExampleCommit {
		#id : ''0a498c8884b1b753135e5d33228a86e191d37f80'',
		#comment : ''Fixes #57 - Iceberg #update should register the created repository\n'',
		#parentIds : [
			''3fa8f6af583d6bacf6ed801e47494c74279151de''
		]
	},
	HiExampleCommit {
		#id : ''3fa8f6af583d6bacf6ed801e47494c74279151de'',
		#comment : ''Fixes #63, do not break if no repository selected.\n'',
		#parentIds : [
			''5e1f11cf975e0adc913c0fa0d7726e7b10dc1636''
		]
	},
	HiExampleCommit {
		#id : ''5e1f11cf975e0adc913c0fa0d7726e7b10dc1636'',
		#comment : ''Method had no references, fixes #61\n'',
		#parentIds : [
			''d7ffa210a5705c0a40b8feaab1ec496b913e5cb1''
		]
	},
	HiExampleCommit {
		#id : ''d7ffa210a5705c0a40b8feaab1ec496b913e5cb1'',
		#comment : ''Ensure branch is no nil when asked. Fix tests.\n'',
		#parentIds : [
			''c8f5c574174f1a5bab253a7fed8f4021d592d513''
		]
	},
	HiExampleCommit {
		#id : ''c8f5c574174f1a5bab253a7fed8f4021d592d513'',
		#comment : ''Ensure branch is no nil when asked. Fix tests.\n'',
		#parentIds : [
			''007e0c1fd9ee06d38e1ae7c2cdd4d0007e4f6d9d''
		]
	},
	HiExampleCommit {
		#id : ''007e0c1fd9ee06d38e1ae7c2cdd4d0007e4f6d9d'',
		#comment : ''Fix testUpdatePackage, add some needed repository refreshes, use pull method instead of command:  \''pull\''\n'',
		#parentIds : [
			''c40ae5205718588becf1d94541b9b26e93aafa11''
		]
	},
	HiExampleCommit {
		#id : ''c40ae5205718588becf1d94541b9b26e93aafa11'',
		#comment : ''Fix testUpdatePackage, add some needed repository refreshes, use pull method instead of command:  \''pull\''\n'',
		#parentIds : [
			''db8a9002284e4d394aa2e39298677b6e3e28b605''
		]
	},
	HiExampleCommit {
		#id : ''db8a9002284e4d394aa2e39298677b6e3e28b605'',
		#comment : ''Fix test \\"testCommitIsLoaded\\"\n'',
		#parentIds : [
			''69c17fce33a16e61e264be813583d0d4048be433''
		]
	},
	HiExampleCommit {
		#id : ''69c17fce33a16e61e264be813583d0d4048be433'',
		#comment : ''Merge branch \''master\'' of github.com:npasserini/iceberg\n'',
		#parentIds : [
			''44ab6af10726fc8be3c958c60f0246eaaf0623b1'',
			''c47f957843023519c30f0dc677879d86b1000534''
		]
	},
	HiExampleCommit {
		#id : ''44ab6af10726fc8be3c958c60f0246eaaf0623b1'',
		#comment : ''Rename some methods, improve naming consistency.\rFix bug in loadSavedPackages.\rImprove delegation from Repositoryto SavedPackage.\n'',
		#parentIds : [
			''5678048359b6401fbcdbd5f717d8d137d71735df''
		]
	},
	HiExampleCommit {
		#id : ''5678048359b6401fbcdbd5f717d8d137d71735df'',
		#comment : ''Rename some methods, improve naming consistency.\rFix bug in loadSavedPackages.\rImprove delegation from Repositoryto SavedPackage.\n'',
		#parentIds : [
			''092a5ff370c4d0d2669827f533df12b838869c1d''
		]
	},
	HiExampleCommit {
		#id : ''c47f957843023519c30f0dc677879d86b1000534'',
		#comment : ''Fixing some lookup of versions.\n'',
		#parentIds : [
			''092a5ff370c4d0d2669827f533df12b838869c1d''
		]
	},
	HiExampleCommit {
		#id : ''092a5ff370c4d0d2669827f533df12b838869c1d'',
		#comment : ''Fixes: #49 - Minimal working GUI for creating repositories.\rAlso added a refresh button for the GUI.\n'',
		#parentIds : [
			''205cc48512fd539cd134e64ec8f0ec840a118eff''
		]
	},
	HiExampleCommit {
		#id : ''205cc48512fd539cd134e64ec8f0ec840a118eff'',
		#comment : ''Fixes #62 - Load / unload package from GUI>\rImprovements on gui repository creation, but still not working.\rRefactored package loading to give (a bit) more responsibility to SavedPackage\n'',
		#parentIds : [
			''06ea153890ca8d5a5136f61c112177a69aeb63b0''
		]
	},
	HiExampleCommit {
		#id : ''06ea153890ca8d5a5136f61c112177a69aeb63b0'',
		#comment : ''Add subdirectory feature.\n'',
		#parentIds : [
			''cd5e127fbc45d23f857e01064bf78c6aff5fc97e''
		]
	},
	HiExampleCommit {
		#id : ''cd5e127fbc45d23f857e01064bf78c6aff5fc97e'',
		#comment : ''Create repository, handle errors, close window.\n'',
		#parentIds : [
			''4cb42b39f15b8de269692daf68f9f94b5b9f11a1''
		]
	},
	HiExampleCommit {
		#id : ''4cb42b39f15b8de269692daf68f9f94b5b9f11a1'',
		#comment : ''Hook repositories browser to new repository creation, and little refactors.\n'',
		#parentIds : [
			''0304581bf825ab3062c5f45ba623f88c6eceb61f''
		]
	},
	HiExampleCommit {
		#id : ''0304581bf825ab3062c5f45ba623f88c6eceb61f'',
		#comment : ''Added (still not working) GUI for creating repositories.\rRenamed tag Glamour to \''GUI\''.\n'',
		#parentIds : [
			''6b350a291de75ca955fa1f485ecf430c3ca7fa2d''
		]
	},
	HiExampleCommit {
		#id : ''6b350a291de75ca955fa1f485ecf430c3ca7fa2d'',
		#comment : ''Fix download instructions.\n'',
		#parentIds : [
			''5dfa7a9c69c268a3f23192e306a34ba7fd1231ea''
		]
	},
	HiExampleCommit {
		#id : ''5dfa7a9c69c268a3f23192e306a34ba7fd1231ea'',
		#comment : ''Merge branch \''master\'' of github.com:npasserini/iceberg\n'',
		#parentIds : [
			''77d0d30892923b6c5c7844b317db60e43422daef'',
			''cba10e18546b7b860d66d4aacbc227b5a3ed74bc''
		]
	},
	HiExampleCommit {
		#id : ''77d0d30892923b6c5c7844b317db60e43422daef'',
		#comment : ''Performance improvements.\n'',
		#parentIds : [
			''fef1770ecf675d0a87b38e645d779af5a21b9169''
		]
	},
	HiExampleCommit {
		#id : ''cba10e18546b7b860d66d4aacbc227b5a3ed74bc'',
		#comment : ''Add menu option to open repository.\n'',
		#parentIds : [
			''89cf187c03a7e08b3060826ca6e9013d339e076f''
		]
	},
	HiExampleCommit {
		#id : ''89cf187c03a7e08b3060826ca6e9013d339e076f'',
		#comment : ''Update repository status on branch change.\n'',
		#parentIds : [
			''fef1770ecf675d0a87b38e645d779af5a21b9169''
		]
	},
	HiExampleCommit {
		#id : ''fef1770ecf675d0a87b38e645d779af5a21b9169'',
		#comment : ''Fixes #55\n'',
		#parentIds : [
			''990c4c5e66acc71dbe97177396a22fcb7feadd2a''
		]
	},
	HiExampleCommit {
		#id : ''990c4c5e66acc71dbe97177396a22fcb7feadd2a'',
		#comment : ''Fixes #54\n'',
		#parentIds : [
			''5718fc0401be6da678c103151e757b10b48f04a6''
		]
	},
	HiExampleCommit {
		#id : ''5718fc0401be6da678c103151e757b10b48f04a6'',
		#comment : ''Refresh repository data after commit.\n'',
		#parentIds : [
			''95f87331aa9359bfd9153a00a864cc84895ea47d''
		]
	},
	HiExampleCommit {
		#id : ''95f87331aa9359bfd9153a00a864cc84895ea47d'',
		#comment : ''Fixes: #53\n'',
		#parentIds : [
			''8b0c3fb4ae51ee43ddc89fad31a646b59c466e7e''
		]
	},
	HiExampleCommit {
		#id : ''8b0c3fb4ae51ee43ddc89fad31a646b59c466e7e'',
		#comment : ''Fixes #50: GUI for creating repositories.\nImprove the way we compute if a commit is already loaded in the image.\n'',
		#parentIds : [
			''62563f88015da4558423bb77662b1605460762e9''
		]
	},
	HiExampleCommit {
		#id : ''62563f88015da4558423bb77662b1605460762e9'',
		#comment : ''Create a repository from repository browser (fixes: #49).\rAdd a repository registry (fixes #33).\rDetect the loadedVersion for a package which was loaded from an MCGitHubRepository.\n'',
		#parentIds : [
			''40ae67bf10a22eca8c1ed0d9d7d4e3e0f662d66c''
		]
	},
	HiExampleCommit {
		#id : ''40ae67bf10a22eca8c1ed0d9d7d4e3e0f662d66c'',
		#comment : ''Fixes #45. Show outgoing versions. Add open synchronizer action.\n'',
		#parentIds : [
			''e8a9d567d7178905d28dae97a53ef752210d2d5e''
		]
	},
	HiExampleCommit {
		#id : ''e8a9d567d7178905d28dae97a53ef752210d2d5e'',
		#comment : ''Show incomming commits in package status.\n'',
		#parentIds : [
			''cc0a0ca27548a9d66d98c48cc180da416e1cc100''
		]
	},
	HiExampleCommit {
		#id : ''cc0a0ca27548a9d66d98c48cc180da416e1cc100'',
		#comment : ''Create ice saved package to model the information about a package in a repository.\n'',
		#parentIds : [
			''503061d2d9720d81be19bcd077afb301f81fb103''
		]
	},
	HiExampleCommit {
		#id : ''503061d2d9720d81be19bcd077afb301f81fb103'',
		#comment : ''Fix initialize method.\n'',
		#parentIds : [
			''7badb6e11d339a68800ddbbffd7e586510b6081f''
		]
	},
	HiExampleCommit {
		#id : ''7badb6e11d339a68800ddbbffd7e586510b6081f'',
		#comment : ''First version of the repositories browser.\rChange the way we compute changes for a package.\n'',
		#parentIds : [
			''560f525d5c1fab16527cd24d621053b4cc95312b''
		]
	},
	HiExampleCommit {
		#id : ''560f525d5c1fab16527cd24d621053b4cc95312b'',
		#comment : ''Fix hierarchy of IcePuched announcement.\n'',
		#parentIds : [
			''990b64129b95846401b5daaa9a3ebec0f7b0c6a8''
		]
	},
	HiExampleCommit {
		#id : ''990b64129b95846401b5daaa9a3ebec0f7b0c6a8'',
		#comment : ''Fix IceCommited hierarchy.\rUse events to clear the cache on commit.\n'',
		#parentIds : [
			''69f1c8070ed25de7ff4cf8eef363df7b5bc055d8''
		]
	},
	HiExampleCommit {
		#id : ''69f1c8070ed25de7ff4cf8eef363df7b5bc055d8'',
		#comment : ''Fix IceCommited hierarchy.\n'',
		#parentIds : [
			''6138ad386365cff6271abef597d4632b5e436a65''
		]
	},
	HiExampleCommit {
		#id : ''6138ad386365cff6271abef597d4632b5e436a65'',
		#comment : ''Add IceCommited event listener.\n'',
		#parentIds : [
			''4c2507c7acf0fa922a655dcbf3122ed03235d107''
		]
	},
	HiExampleCommit {
		#id : ''4c2507c7acf0fa922a655dcbf3122ed03235d107'',
		#comment : ''Add \\"Publish Browser\\" view.\rCompute outgoing commits.\rAdd push functionality to IceRepository.\rAdd IcePush announcement.\n'',
		#parentIds : [
			''423bedeaf5350c383ab23d6a05abe23d48374405''
		]
	},
	HiExampleCommit {
		#id : ''423bedeaf5350c383ab23d6a05abe23d48374405'',
		#comment : ''Fix typo.\n'',
		#parentIds : [
			''5525e2a0b26ac4e699b0f27fc3a1abf472ed5fa5''
		]
	},
	HiExampleCommit {
		#id : ''5525e2a0b26ac4e699b0f27fc3a1abf472ed5fa5'',
		#comment : ''Simplify changes tree.\n'',
		#parentIds : [
			''5068bcd283a447295986b990356dfa426b70d467''
		]
	},
	HiExampleCommit {
		#id : ''5068bcd283a447295986b990356dfa426b70d467'',
		#comment : ''Move refresh button and a bit of clean up.\n'',
		#parentIds : [
			''fd34a0d0b1959b844e3243683a79a02d0671b077''
		]
	},
	HiExampleCommit {
		#id : ''fd34a0d0b1959b844e3243683a79a02d0671b077'',
		#comment : ''Update selected commits and update the \''update browser\'' view.\rAdd an announcement to mark when a package is loaded.\n'',
		#parentIds : [
			''bc4ec0acd5e702b852646da30bdfe439dbe3a898''
		]
	},
	HiExampleCommit {
		#id : ''bc4ec0acd5e702b852646da30bdfe439dbe3a898'',
		#comment : ''Show differences between snapshots.\n'',
		#parentIds : [
			''71e7d2e1075a12afde0c08cd485ac9ae5cab49da''
		]
	},
	HiExampleCommit {
		#id : ''71e7d2e1075a12afde0c08cd485ac9ae5cab49da'',
		#comment : ''Better title for synchronizer\n'',
		#parentIds : [
			''444807b3065daa80c0305f52c54e024f96517161''
		]
	},
	HiExampleCommit {
		#id : ''444807b3065daa80c0305f52c54e024f96517161'',
		#comment : ''Renamed IceDiff to IceAbstractDiff and IceWorkingCopyDiff to IceDiff.\rSeparated the destination of a diff into an independent object, created the hierarchy of IceCommitish to model those objects.\rChanged model of IceGlamourSynchronizer from itself to just the repository object, better separation of model and view (before was impossible).\r\rPartial creation of diffs between image (working copy) and arbirtrary commit.\n'',
		#parentIds : [
			''95df8f6d19e6801906e65513ad250f2fcd575907''
		]
	},
	HiExampleCommit {
		#id : ''95df8f6d19e6801906e65513ad250f2fcd575907'',
		#comment : ''Renamed IceDiff to IceAbstractDiff and IceWorkingCopyDiff to IceDiff.\rSeparated the destination of a diff into an independent object, created the hierarchy of IceCommitish to model those objects.\rChanged model of IceGlamourSynchronizer from itself to just the repository object, better separation of model and view (before was impossible).\r\rPartial creation of diffs between image (working copy) and arbirtrary commit.\n'',
		#parentIds : [
			''e935dba8f0cc09a3741e04e858cfcb10a6bead77''
		]
	},
	HiExampleCommit {
		#id : ''e935dba8f0cc09a3741e04e858cfcb10a6bead77'',
		#comment : ''Improvements in update view, show incomming commits. Required a new commit model.\n'',
		#parentIds : [
			''9b1c992d6385dcf7f517b956dfc65fbee30fd05a''
		]
	},
	HiExampleCommit {
		#id : ''9b1c992d6385dcf7f517b956dfc65fbee30fd05a'',
		#comment : ''Improvements in update view, show incomming commits. Required a new commit model.\n'',
		#parentIds : [
			''705bbe83ba6afab251ea47e61b10d88da21b002e''
		]
	},
	HiExampleCommit {
		#id : ''705bbe83ba6afab251ea47e61b10d88da21b002e'',
		#comment : ''Add \''update browser\'' to the IceSynchronizer.\rAdd IceCommitInfo to model the info of past commits.\n'',
		#parentIds : [
			''31592217dacf7234cca647962545a4bbef9367fd''
		]
	},
	HiExampleCommit {
		#id : ''31592217dacf7234cca647962545a4bbef9367fd'',
		#comment : ''Add \''update browser\'' to the IceSynchronizer.\rAdd IceCommitInfo to model the info of past commits.\n'',
		#parentIds : [
			''2a76aa949ceca5fad1e3cda90e802648536385a3''
		]
	},
	HiExampleCommit {
		#id : ''2a76aa949ceca5fad1e3cda90e802648536385a3'',
		#comment : ''Refactor ChangesTree as an independent component.\rAdd text to action bar buttons.\n'',
		#parentIds : [
			''e5e9f59562276c50e57a04157aec495d20b4797b''
		]
	},
	HiExampleCommit {
		#id : ''e5e9f59562276c50e57a04157aec495d20b4797b'',
		#comment : ''Fix method categorization.\n'',
		#parentIds : [
			''8fbd95af57c7140898bbe637c808c1fd25812508''
		]
	},
	HiExampleCommit {
		#id : ''8fbd95af57c7140898bbe637c808c1fd25812508'',
		#comment : ''Replace action list for action bar icons.\n'',
		#parentIds : [
			''68c99942cb8cf9c9f614688218afc15036f7e173''
		]
	},
	HiExampleCommit {
		#id : ''68c99942cb8cf9c9f614688218afc15036f7e173'',
		#comment : ''Improvements in refresh changes.\n'',
		#parentIds : [
			''7760e961952f8f9f9b5c6660195c5af521a5c81a''
		]
	},
	HiExampleCommit {
		#id : ''7760e961952f8f9f9b5c6660195c5af521a5c81a'',
		#comment : ''First commit with glamour synchronizer.\n'',
		#parentIds : [
			''1bd8917d53286418cfbee38e631c4e6e7888bc94''
		]
	},
	HiExampleCommit {
		#id : ''1bd8917d53286418cfbee38e631c4e6e7888bc94'',
		#comment : ''Show diffs\n'',
		#parentIds : [
			''cc1840622b0ee084659605969dd23542e689ef95''
		]
	},
	HiExampleCommit {
		#id : ''cc1840622b0ee084659605969dd23542e689ef95'',
		#comment : ''Working independent glamour-based CommitBrowser and Synchronizer.\n'',
		#parentIds : [
			''7ab8f131503045ef8bbd84465e77414ebd76a108''
		]
	},
	HiExampleCommit {
		#id : ''7ab8f131503045ef8bbd84465e77414ebd76a108'',
		#comment : ''Refactor commit browser as an independent component.\n'',
		#parentIds : [
			''6fb8b591b07c700e8336569e9e9d7ce155e9a08b''
		]
	},
	HiExampleCommit {
		#id : ''6fb8b591b07c700e8336569e9e9d7ce155e9a08b'',
		#comment : ''New glamour-based synchronizer (work in progress).\n'',
		#parentIds : [
			''ee091329aaa37805b9fa0bac2f343d7d97465fb2''
		]
	},
	HiExampleCommit {
		#id : ''ee091329aaa37805b9fa0bac2f343d7d97465fb2'',
		#comment : ''First attempt to a new sinchronizer including pull/push, with spec.\n'',
		#parentIds : [
			''c15421e4b8231749dd7498c46c3aa79174f595c0''
		]
	},
	HiExampleCommit {
		#id : ''c15421e4b8231749dd7498c46c3aa79174f595c0'',
		#comment : ''Partial solution for #5, handles tags creation and removals.\n'',
		#parentIds : [
			''e2aef796bcf316bf33a4ba208c4f40c5b52c4af2''
		]
	},
	HiExampleCommit {
		#id : ''e2aef796bcf316bf33a4ba208c4f40c5b52c4af2'',
		#comment : ''Partial solution for #5, handles tags creation and removals.\n'',
		#parentIds : [
			''a28e63a33c49ba325aa0492efac216f074aad6a7''
		]
	},
	HiExampleCommit {
		#id : ''a28e63a33c49ba325aa0492efac216f074aad6a7'',
		#comment : ''Fix syntax problem\n'',
		#parentIds : [
			''3ba35ebb1f91a87f3fea6ce6e0f7e215268bfc18''
		]
	},
	HiExampleCommit {
		#id : ''3ba35ebb1f91a87f3fea6ce6e0f7e215268bfc18'',
		#comment : ''Update documentation, adapt it to the new repository URL\n'',
		#parentIds : [
			''c9aa858a7ef5352565ee62da1bc21d746a80e9a7''
		]
	},
	HiExampleCommit {
		#id : ''c9aa858a7ef5352565ee62da1bc21d746a80e9a7'',
		#comment : ''Fixes #22, fixes #40.\n'',
		#parentIds : [
			''175729e26484019739688fca62de0b986098d032''
		]
	},
	HiExampleCommit {
		#id : ''175729e26484019739688fca62de0b986098d032'',
		#comment : ''Fixes #35 - You can get an editable repository for an existing package by  Ì\u0080 repo := anRPackage iceRepository`.\n'',
		#parentIds : [
			''a60355786647ace714999d1946c5dbf49392d8ca''
		]
	},
	HiExampleCommit {
		#id : ''a60355786647ace714999d1946c5dbf49392d8ca'',
		#comment : ''Fixes #35 - You can get an editable repository for an existing package by  Ì\u0080 repo := anRPackage iceRepository`.\n'',
		#parentIds : [
			''80f59181fdc2445cdb42963d17a0e6f63d5a945e''
		]
	},
	HiExampleCommit {
		#id : ''80f59181fdc2445cdb42963d17a0e6f63d5a945e'',
		#comment : ''Fix broken test.\rAdd removal of Baseline package.\rAdd new synchronizer example, over the test package.\n'',
		#parentIds : [
			''46ddabd7bb2f3377490b2a92bd43707b2b0de42a''
		]
	},
	HiExampleCommit {
		#id : ''46ddabd7bb2f3377490b2a92bd43707b2b0de42a'',
		#comment : ''Fix broken test.\rAdd removal of Baseline package.\rAdd new synchronizer example, over the test package.\n'',
		#parentIds : [
			''66c05ceeb730caa98e516b428b0cfc43ba7a9db0''
		]
	},
	HiExampleCommit {
		#id : ''66c05ceeb730caa98e516b428b0cfc43ba7a9db0'',
		#comment : ''Fixes #8\rRename class Ice to Iceberg.\n'',
		#parentIds : [
			''b1197fd19c1570dc6f0b3bc58d501313304c2c45''
		]
	},
	HiExampleCommit {
		#id : ''b1197fd19c1570dc6f0b3bc58d501313304c2c45'',
		#comment : ''Fixes #8\rRename class Ice to Iceberg.\n'',
		#parentIds : [
			''1a050ed8a5ca2f260bd5fc6db7dc0e2bcc66e51f''
		]
	},
	HiExampleCommit {
		#id : ''1a050ed8a5ca2f260bd5fc6db7dc0e2bcc66e51f'',
		#comment : ''Fixes #37, #28\r\rAlso remove dummy test methods.\n'',
		#parentIds : [
			''6f1ca2d21e5d7cd1107745576f2a8bb0ab414272''
		]
	},
	HiExampleCommit {
		#id : ''6f1ca2d21e5d7cd1107745576f2a8bb0ab414272'',
		#comment : ''Fixes #36\r- Use RPackageOrganization instead of MCWorkingCopy to get the package out of a tag/category.\r- IceWorkingCopyDiff now ignores unloaded packages.\r- Refactor how to compute iceClass / icePackage as method extensions have a package that is different from the one of the class.\n'',
		#parentIds : [
			''901b203fd9ef0a5fa11ac9edb4a65dc78f9e9d21''
		]
	},
	HiExampleCommit {
		#id : ''901b203fd9ef0a5fa11ac9edb4a65dc78f9e9d21'',
		#comment : ''Fixes #36\r- Use RPackageOrganization instead of MCWorkingCopy to get the package out of a tag/category.\r- IceWorkingCopyDiff now ignores unloaded packages.\r- Refactor how to compute iceClass / icePackage as method extensions have a package that is different from the one of the class.\n'',
		#parentIds : [
			''31c50cf95aa282011ad29ccf2e4599df34e3ab66''
		]
	},
	HiExampleCommit {
		#id : ''31c50cf95aa282011ad29ccf2e4599df34e3ab66'',
		#comment : ''Fixes #6\r- Add abstract class for common test code.\r- Add IcePackageReference to model packages from our view.\r- Differentiate loaded from unloaded packages in a repository.\r-\n'',
		#parentIds : [
			''8a780ab76664771b172fbc8c4fe32839d0847dbd''
		]
	},
	HiExampleCommit {
		#id : ''8a780ab76664771b172fbc8c4fe32839d0847dbd'',
		#comment : ''Fixes #6\r- Add abstract class for common test code.\r- Add IcePackageReference to model packages from our view.\r- Differentiate loaded from unloaded packages in a repository.\r-\n'',
		#parentIds : [
			''694b892d0d4396df6578faf42a277caa91af959a''
		]
	},
	HiExampleCommit {
		#id : ''694b892d0d4396df6578faf42a277caa91af959a'',
		#comment : ''New readme for v0.0.3\n'',
		#parentIds : [
			''f7e632db38003e9bea8d3e2a3acb96abeff1f4e5''
		]
	},
	HiExampleCommit {
		#id : ''f7e632db38003e9bea8d3e2a3acb96abeff1f4e5'',
		#comment : ''Fixes #29\n'',
		#parentIds : [
			''c5b0872ee631437ba7d3dbe540a2d3e2d57f613e''
		]
	},
	HiExampleCommit {
		#id : ''c5b0872ee631437ba7d3dbe540a2d3e2d57f613e'',
		#comment : ''Fixes #19\n'',
		#parentIds : [
			''e2e88f99d2cbaac5dc3bc27d6dda2ea6ca51d1ec''
		]
	},
	HiExampleCommit {
		#id : ''e2e88f99d2cbaac5dc3bc27d6dda2ea6ca51d1ec'',
		#comment : ''Ensure the creation of the iceberg-cache directory structure.\rImprove description of Iceberg MC repositories.\n'',
		#parentIds : [
			''31994096ff823b5a553351428d42a51987e5510d''
		]
	},
	HiExampleCommit {
		#id : ''31994096ff823b5a553351428d42a51987e5510d'',
		#comment : ''Add new test: testUseExistingLocalRepo\n'',
		#parentIds : [
			''442bf5069669d5d4a5d2ed958f3e7bbab1600fbd''
		]
	},
	HiExampleCommit {
		#id : ''442bf5069669d5d4a5d2ed958f3e7bbab1600fbd'',
		#comment : ''Fix tests.\n'',
		#parentIds : [
			''e10d5bee2e5cdda3829e753fb5aa93ba883ef08e''
		]
	},
	HiExampleCommit {
		#id : ''e10d5bee2e5cdda3829e753fb5aa93ba883ef08e'',
		#comment : ''Fix tests.\n'',
		#parentIds : [
			''2b3f4d17e721bd1d1023c6ece8c70f294beb253c''
		]
	},
	HiExampleCommit {
		#id : ''2b3f4d17e721bd1d1023c6ece8c70f294beb253c'',
		#comment : ''Allow to create repositories in locations different from the default one.\rFirst attempt to re-use existing local repositories.\n'',
		#parentIds : [
			''8960c8bf5de84ab32d56a1765cae933b609e87cb''
		]
	},
	HiExampleCommit {
		#id : ''8960c8bf5de84ab32d56a1765cae933b609e87cb'',
		#comment : ''Allow to create repositories in locations different from the default one.\rFirst attempt to re-use existing local repositories.\n'',
		#parentIds : [
			''271daccab39acf47d132ef2d18bfa7a93edd4592''
		]
	},
	HiExampleCommit {
		#id : ''271daccab39acf47d132ef2d18bfa7a93edd4592'',
		#comment : ''Create a new type of Monticello repository, to decouple Iceberg from Monticello and GitFileTree specifics\n'',
		#parentIds : [
			''7828567f9646ac30ab642aba3fdcf88af0ad08fa''
		]
	},
	HiExampleCommit {
		#id : ''7828567f9646ac30ab642aba3fdcf88af0ad08fa'',
		#comment : ''Fix package names in baseline.\n'',
		#parentIds : [
			''d51ccde60b4f3c847eaf9f77ebf4615cdf89ae3a''
		]
	},
	HiExampleCommit {
		#id : ''d51ccde60b4f3c847eaf9f77ebf4615cdf89ae3a'',
		#comment : ''Update baseline to use own fork of gitFileTree.\n'',
		#parentIds : [
			''edb88a81f65e0bb0d22e9f44ceb11b06994c29d2''
		]
	},
	HiExampleCommit {
		#id : ''edb88a81f65e0bb0d22e9f44ceb11b06994c29d2'',
		#comment : ''Changed default location of local clones to iceberg-cache/{repositoryOwner}/{projectName}.\r\rAdded IceRemoteRepository, which for now simply knows how to extract some information from the repository url.\n'',
		#parentIds : [
			''858c6a48ec119a8b412266ceab5375893909521b''
		]
	},
	HiExampleCommit {
		#id : ''858c6a48ec119a8b412266ceab5375893909521b'',
		#comment : ''Changed default location of local clones to iceberg-cache/{repositoryOwner}/{projectName}.\r\rAdded IceRemoteRepository, which for now simply knows how to extract some information from the repository url.\n'',
		#parentIds : [
			''2710a67085cc5b2cc862887e0f06bce5d23409eb''
		]
	},
	HiExampleCommit {
		#id : ''2710a67085cc5b2cc862887e0f06bce5d23409eb'',
		#comment : ''Renamed test class.\n'',
		#parentIds : [
			''1d5a1102ff4bff281d23fdcbd941682d5ec0c352''
		]
	},
	HiExampleCommit {
		#id : ''1d5a1102ff4bff281d23fdcbd941682d5ec0c352'',
		#comment : ''Renamed test class.\n'',
		#parentIds : [
			''5e491dd363f7e57342bfe8414ecdefe2e19f0223''
		]
	},
	HiExampleCommit {
		#id : ''5e491dd363f7e57342bfe8414ecdefe2e19f0223'',
		#comment : ''Reorganized classes using new tag Iceberg-Changes.\rAlso corrected some method categorization.\n'',
		#parentIds : [
			''5d82d617af3da4362f97c9b537fa4a4a41ccb255''
		]
	},
	HiExampleCommit {
		#id : ''5d82d617af3da4362f97c9b537fa4a4a41ccb255'',
		#comment : ''Created IceClassReferences to allow for polymorphism between a reference to an existing Pharo class and a removed class. Now the change sets use these objects to represent classes\n'',
		#parentIds : [
			''382e163d3d2d338cd7b1bd34e3027b5620594aaa''
		]
	},
	HiExampleCommit {
		#id : ''382e163d3d2d338cd7b1bd34e3027b5620594aaa'',
		#comment : ''Update diff when refreshing chagnes.\n'',
		#parentIds : [
			''0d40764a959933066c7beafa6a9c116c90cd57f4''
		]
	},
	HiExampleCommit {
		#id : ''0d40764a959933066c7beafa6a9c116c90cd57f4'',
		#comment : ''Fix refresh of the tree, also refresh tree on commit.\n'',
		#parentIds : [
			''6a41abc1d719bf3a7c0377ee37a24cc4f977f9a7''
		]
	},
	HiExampleCommit {
		#id : ''6a41abc1d719bf3a7c0377ee37a24cc4f977f9a7'',
		#comment : ''Removed renamed packages\n'',
		#parentIds : [
			''ccf898625b6278a23c68311a34760cf93ded4ae1''
		]
	},
	HiExampleCommit {
		#id : ''ccf898625b6278a23c68311a34760cf93ded4ae1'',
		#comment : ''Rename.\n'',
		#parentIds : [
			''0e65a2a6f6a4642d11a0cfac26b801e965d9e6cb''
		]
	},
	HiExampleCommit {
		#id : ''0e65a2a6f6a4642d11a0cfac26b801e965d9e6cb'',
		#comment : ''Rename package\n'',
		#parentIds : [
			''e8e9b85a103770ff2b1d4fe2d37e4d882e87ae95''
		]
	},
	HiExampleCommit {
		#id : ''e8e9b85a103770ff2b1d4fe2d37e4d882e87ae95'',
		#comment : ''Refresh the changes tree after commiting.\n'',
		#parentIds : [
			''be630bcdcdacd43f80d7294270cbf65d96164e96''
		]
	},
	HiExampleCommit {
		#id : ''be630bcdcdacd43f80d7294270cbf65d96164e96'',
		#comment : ''Rename Git class to IceRepository\n'',
		#parentIds : [
			''e86fc045b3387476b218fa9c5780d1be2d403685''
		]
	},
	HiExampleCommit {
		#id : ''e86fc045b3387476b218fa9c5780d1be2d403685'',
		#comment : ''Merge branch \''master\'' of github.com:npasserini/pharo-git\n'',
		#parentIds : [
			''625ac3b1d1d24ee342bb31876a023a547e10b190'',
			''83b0c4a00d300c93415dbf30fa09515acf4d7a51''
		]
	},
	HiExampleCommit {
		#id : ''625ac3b1d1d24ee342bb31876a023a547e10b190'',
		#comment : ''Improve docs\n'',
		#parentIds : [
			''d00f2587b1bec4ac5ee1c6c99a0d38a64ab669b6''
		]
	},
	HiExampleCommit {
		#id : ''83b0c4a00d300c93415dbf30fa09515acf4d7a51'',
		#comment : ''Merge pull request #20 from npasserini/newChangeSets\n\nThis closes #12 and closes #7Replaced changeset model to allow for â\u0080¦'',
		#parentIds : [
			''d00f2587b1bec4ac5ee1c6c99a0d38a64ab669b6'',
			''46d4a7b15bbaecbcebb93f359c498b3d1146ac3b''
		]
	},
	HiExampleCommit {
		#id : ''46d4a7b15bbaecbcebb93f359c498b3d1146ac3b'',
		#comment : ''This closes #12 and closes #7\r\rReplaced changeset model to allow for refreshing the changes tree without reopening the synchronizer window.\r\rShow diff of class definitions.\n'',
		#parentIds : [
			''d00f2587b1bec4ac5ee1c6c99a0d38a64ab669b6''
		]
	},
	HiExampleCommit {
		#id : ''d00f2587b1bec4ac5ee1c6c99a0d38a64ab669b6'',
		#comment : ''Remove halt.\n'',
		#parentIds : [
			''778d9fd81f670230f8f4029cc74a7c114d10af72''
		]
	},
	HiExampleCommit {
		#id : ''778d9fd81f670230f8f4029cc74a7c114d10af72'',
		#comment : ''Fix #18\n'',
		#parentIds : [
			''ef80e507582a5303776fde73337420a27ef410d1''
		]
	},
	HiExampleCommit {
		#id : ''ef80e507582a5303776fde73337420a27ef410d1'',
		#comment : '''',
		#parentIds : [
			''96dd7be744a6493b5e7345f1ad9986474db1f9d2''
		]
	},
	HiExampleCommit {
		#id : ''96dd7be744a6493b5e7345f1ad9986474db1f9d2'',
		#comment : '''',
		#parentIds : [
			''df2f034859eb6105b29896f27b6cb975ce701dbe''
		]
	},
	HiExampleCommit {
		#id : ''df2f034859eb6105b29896f27b6cb975ce701dbe'',
		#comment : '''',
		#parentIds : [
			''eb61818dc2397a8ee567f379a0dda15ee0bec4ee''
		]
	},
	HiExampleCommit {
		#id : ''eb61818dc2397a8ee567f379a0dda15ee0bec4ee'',
		#comment : '''',
		#parentIds : [
			''919cc8f72a0dc710a545553331e1894aac1eaea9''
		]
	},
	HiExampleCommit {
		#id : ''919cc8f72a0dc710a545553331e1894aac1eaea9'',
		#comment : ''Fix baseline: remove unallowed versionString:\n'',
		#parentIds : [
			''a3481011820ad8ada93bb7f71587bb4c4ade28f7''
		]
	},
	HiExampleCommit {
		#id : ''a3481011820ad8ada93bb7f71587bb4c4ade28f7'',
		#comment : '''',
		#parentIds : [
			''89ad569ec003270ee519ad07a129902276341545''
		]
	},
	HiExampleCommit {
		#id : ''89ad569ec003270ee519ad07a129902276341545'',
		#comment : '''',
		#parentIds : [
			''8e201ac568672f7561c1325df9ce8a87013d26ca''
		]
	},
	HiExampleCommit {
		#id : ''8e201ac568672f7561c1325df9ce8a87013d26ca'',
		#comment : ''Save baseline.\n'',
		#parentIds : [
			''5111f891af957d810663c67c53d6c280c55cde0d''
		]
	},
	HiExampleCommit {
		#id : ''5111f891af957d810663c67c53d6c280c55cde0d'',
		#comment : '''',
		#parentIds : [
			''704b90a5537880882fe7a03278e23599fa439eb5''
		]
	},
	HiExampleCommit {
		#id : ''704b90a5537880882fe7a03278e23599fa439eb5'',
		#comment : '''',
		#parentIds : [
			''f239f52ee466c5f01c5a1cccddb4cb324d53cc62''
		]
	},
	HiExampleCommit {
		#id : ''f239f52ee466c5f01c5a1cccddb4cb324d53cc62'',
		#comment : ''Improve instalation instructions\n'',
		#parentIds : [
			''8f427e33b40408a65e53908f96ff548734f03c9f''
		]
	},
	HiExampleCommit {
		#id : ''8f427e33b40408a65e53908f96ff548734f03c9f'',
		#comment : '''',
		#parentIds : [
			''94096846d575c1a221e265f544c2b768b5848ab0''
		]
	},
	HiExampleCommit {
		#id : ''94096846d575c1a221e265f544c2b768b5848ab0'',
		#comment : ''First commit through the Ice GUI\n'',
		#parentIds : [
			''ebb3fad0758f6776320b608dacb9d4314cec263c''
		]
	},
	HiExampleCommit {
		#id : ''ebb3fad0758f6776320b608dacb9d4314cec263c'',
		#comment : ''Separated extension methods from the rest of the changes in a package (we might rollback this in the future).\r\rAdded icons to the tree.\n'',
		#parentIds : [
			''40713a852a43b424fa012bf36279fa43b36026ac''
		]
	},
	HiExampleCommit {
		#id : ''40713a852a43b424fa012bf36279fa43b36026ac'',
		#comment : ''Improvements in tree rendering\r\r- Introduced IceSimpleChangeSet, separated from StructuralChangeSet. \r- Temporarily removed checklists from the changes treeview.\n'',
		#parentIds : [
			''4ed4c492d90a262e8c95383474146a26e7348b78''
		]
	},
	HiExampleCommit {
		#id : ''4ed4c492d90a262e8c95383474146a26e7348b78'',
		#comment : ''Add some views:\r- a list of working copies in a repository\r- a tree of changes\r\rAdd IceChangeSet hierarchy, a composite representing the changes between two vesions (more specifically between two snapshosts).\n'',
		#parentIds : [
			''cdc030c952403a2104b1ec6e8328f3198515fb73''
		]
	},
	HiExampleCommit {
		#id : ''cdc030c952403a2104b1ec6e8328f3198515fb73'',
		#comment : ''Rename PackageList => IceWorkingCopyList.\r\r#allPackages method returns only packages and not working copies.\r\rAdd method in Ice class to retrieve working copies for package in a repository. (Dubious mehtod, but I think that maybe is cleaner that repositories have no access to working copies... this will be reviewed.)\n'',
		#parentIds : [
			''d490f6ef2a9fc94e82b431b3bc5067fe44848957''
		]
	},
	HiExampleCommit {
		#id : ''d490f6ef2a9fc94e82b431b3bc5067fe44848957'',
		#comment : ''New #allPackages method returns a list of MCWorkingCopy (has to be renamed). \r\rCreated class Ice to simplify creation of Ice objects.\r\rFirst introduction of Ice prefix (pending rename of pre-existing classes).\n'',
		#parentIds : [
			''598aefd7406e9746158375b048cf129e30ba8c44''
		]
	},
	HiExampleCommit {
		#id : ''598aefd7406e9746158375b048cf129e30ba8c44'',
		#comment : ''Readme\n'',
		#parentIds : [
			''8e57b7421db78876845dd0f3099cef4fba5c5f8f''
		]
	},
	HiExampleCommit {
		#id : ''8e57b7421db78876845dd0f3099cef4fba5c5f8f'',
		#comment : ''First steps into a UI.\n'',
		#parentIds : [
			''aac592e7b3d3bc17f159064db0c71114089a6b03''
		]
	},
	HiExampleCommit {
		#id : ''aac592e7b3d3bc17f159064db0c71114089a6b03'',
		#comment : ''Restore\n'',
		#parentIds : [
			''4ae70f6400e923700e790cac0bda68bb27db6534''
		]
	},
	HiExampleCommit {
		#id : ''4ae70f6400e923700e790cac0bda68bb27db6534'',
		#comment : ''Restore\n'',
		#parentIds : [
			''c626810ed3ac12afc842710ee56ea3aa1b6d1373''
		]
	},
	HiExampleCommit {
		#id : ''c626810ed3ac12afc842710ee56ea3aa1b6d1373'',
		#comment : ''Restore.\n'',
		#parentIds : [
			''60054cbcfbdcb370779afd6812f1c1d53caa91d5''
		]
	},
	HiExampleCommit {
		#id : ''60054cbcfbdcb370779afd6812f1c1d53caa91d5'',
		#comment : ''Restore.\n'',
		#parentIds : [
			''6880ab42ddc80f834f205666b5938093fae62223''
		]
	},
	HiExampleCommit {
		#id : ''6880ab42ddc80f834f205666b5938093fae62223'',
		#comment : ''Update package\n'',
		#parentIds : [
			''be9f33c509d61ce45f98c32c452067b0f910e35a''
		]
	},
	HiExampleCommit {
		#id : ''be9f33c509d61ce45f98c32c452067b0f910e35a'',
		#comment : ''Tests for update package.\n'',
		#parentIds : [
			''85c856a396d00ad5101d741f3c3409878c313968''
		]
	},
	HiExampleCommit {
		#id : ''85c856a396d00ad5101d741f3c3409878c313968'',
		#comment : ''Update package.\n'',
		#parentIds : [
			''63ae3eb6537f5a86eb4220eb4da0a3e45da72bcb''
		]
	},
	HiExampleCommit {
		#id : ''63ae3eb6537f5a86eb4220eb4da0a3e45da72bcb'',
		#comment : ''1) Do not use an instance of a repository just for executing a git command.\r2) Fix repository creation (was confusing branches with directories).\r3) Expect a mandatory commit message before saving a package.\n'',
		#parentIds : [
			''a4c9bab40b283835170228fade58c8ef57b10050''
		]
	},
	HiExampleCommit {
		#id : ''a4c9bab40b283835170228fade58c8ef57b10050'',
		#comment : ''Add automatic commit message when saving a package.\n'',
		#parentIds : [
			''09b8ed9291515100e141ca7bff1ecf803a79e01b''
		]
	},
	HiExampleCommit {
		#id : ''09b8ed9291515100e141ca7bff1ecf803a79e01b'',
		#comment : ''Improve how we remove test packages\n'',
		#parentIds : [
			''6eb8c115016f31bb907590d3c8b06a9b863cce22''
		]
	},
	HiExampleCommit {
		#id : ''6eb8c115016f31bb907590d3c8b06a9b863cce22'',
		#comment : ''Tests for \''save package\''\n'',
		#parentIds : [
			''cec5a87063b86cf663b0613218f25e0acf3debcc''
		]
	},
	HiExampleCommit {
		#id : ''cec5a87063b86cf663b0613218f25e0acf3debcc'',
		#comment : ''Implement \''save package\''\n'',
		#parentIds : [
			''42f990f826148e16b991c2c6ca5b5e351d752c7c''
		]
	},
	HiExampleCommit {
		#id : ''42f990f826148e16b991c2c6ca5b5e351d752c7c'',
		#comment : ''Fix package removal.\n'',
		#parentIds : [
			''bde591490dcae6d0610a275ea88a0a417c99e344''
		]
	},
	HiExampleCommit {
		#id : ''bde591490dcae6d0610a275ea88a0a417c99e344'',
		#comment : ''Use assert:equals: instead of asser:\n'',
		#parentIds : [
			''0cf13995d883f413e0c687053bd1a3d7eb7436f1''
		]
	},
	HiExampleCommit {
		#id : ''0cf13995d883f413e0c687053bd1a3d7eb7436f1'',
		#comment : ''Test new features.\n'',
		#parentIds : [
			''39ea1946ecef27043d4514475fceb0081eb855dd''
		]
	},
	HiExampleCommit {
		#id : ''39ea1946ecef27043d4514475fceb0081eb855dd'',
		#comment : ''Switch between branches\n'',
		#parentIds : [
			''349e14c27cf27e560e60ef00dd8c310e4399e3a0''
		]
	},
	HiExampleCommit {
		#id : ''349e14c27cf27e560e60ef00dd8c310e4399e3a0'',
		#comment : ''Add asserts for branch creations.\n'',
		#parentIds : [
			''37e111d780009ca66faf0f3c7f4243f9c8e4ec1d''
		]
	},
	HiExampleCommit {
		#id : ''37e111d780009ca66faf0f3c7f4243f9c8e4ec1d'',
		#comment : ''Query current branch name\n'',
		#parentIds : [
			''9ec6fadd8fea322ad0e611d127f7c4980bf1bd8d''
		]
	},
	HiExampleCommit {
		#id : ''9ec6fadd8fea322ad0e611d127f7c4980bf1bd8d'',
		#comment : ''Test create branch functionality.\n'',
		#parentIds : [
			''c42c303d069ef3a1d198b2a1a3498ccf163a367e''
		]
	},
	HiExampleCommit {
		#id : ''c42c303d069ef3a1d198b2a1a3498ccf163a367e'',
		#comment : ''Add create branch functionality.\n'',
		#parentIds : [
			''a3c3d204f7fbf6f87cbec7e76d74a684e4767b43''
		]
	},
	HiExampleCommit {
		#id : ''a3c3d204f7fbf6f87cbec7e76d74a684e4767b43'',
		#comment : ''empty log message\n'',
		#parentIds : [
			''d43549fd7a9a5119bfa5a767b6d6d20b3fb0c216''
		]
	},
	HiExampleCommit {
		#id : ''d43549fd7a9a5119bfa5a767b6d6d20b3fb0c216'',
		#comment : ''empty log message\n'',
		#parentIds : [
			''248bcc8134f7f7ed2c4381022d64e3519777c046''
		]
	},
	HiExampleCommit {
		#id : ''248bcc8134f7f7ed2c4381022d64e3519777c046'',
		#comment : ''Improve class documentation.\n'',
		#parentIds : [
			''e0a9e0d54ec546d4ea9dbffac9bf700b86d6f9d4''
		]
	},
	HiExampleCommit {
		#id : ''e0a9e0d54ec546d4ea9dbffac9bf700b86d6f9d4'',
		#comment : ''Improve class documentation\n'',
		#parentIds : [
			''e690998b6901982c96a2f29b0c723d4690cf5c6e''
		]
	},
	HiExampleCommit {
		#id : ''e690998b6901982c96a2f29b0c723d4690cf5c6e'',
		#comment : ''Class documentation\n'',
		#parentIds : [
			''5919b7ef6450df7311f222f01df36b28ac68ba21''
		]
	},
	HiExampleCommit {
		#id : ''5919b7ef6450df7311f222f01df36b28ac68ba21'',
		#comment : ''Remove test package after test did run\n'',
		#parentIds : [
			''cbc37789b867f4671ebe166684f6221b6250bc4b''
		]
	},
	HiExampleCommit {
		#id : ''cbc37789b867f4671ebe166684f6221b6250bc4b'',
		#comment : ''First test\n'',
		#parentIds : [
			''0a88ad24346a3bac0217a9ee801da960e35d38d9''
		]
	},
	HiExampleCommit {
		#id : ''0a88ad24346a3bac0217a9ee801da960e35d38d9'',
		#comment : ''Improve readme\n'',
		#parentIds : [
			''25ef92abd1855967a37aacc4c0ea9aaa67f911c0''
		]
	},
	HiExampleCommit {
		#id : ''25ef92abd1855967a37aacc4c0ea9aaa67f911c0'',
		#comment : ''Instalation instructions\n'',
		#parentIds : [
			''ead80b2baec92ee4e71a9b3fcfcfde466dd2ec48''
		]
	},
	HiExampleCommit {
		#id : ''ead80b2baec92ee4e71a9b3fcfcfde466dd2ec48'',
		#comment : ''First implementation of package load.\n'',
		#parentIds : [
			''7c1d7c755aab48b00427f94e56b3d75b3bf4d884''
		]
	},
	HiExampleCommit {
		#id : ''7c1d7c755aab48b00427f94e56b3d75b3bf4d884'',
		#comment : ''gitfiletree repository properties\n'',
		#parentIds : [ ]
	}
]')
]

{ #category : #examples }
HiExampleCommit class >> pharo_v60507 [
	^ STON fromString: ('OrderedCollection [
	HiExampleCommit {
		#id : ''fc92bbe92e5cfec1f815a6bc7bcde396c722a901'',
		#comment : ''EXPORT VERSION 60507\n'',
		#parentIds : [
			''d51d99e403f8bf9bb941d8d9d773875c063a963b''
		]
	},
	HiExampleCommit {
		#id : ''d51d99e403f8bf9bb941d8d9d773875c063a963b'',
		#comment : ''EXPORT VERSION 60506\n'',
		#parentIds : [
			''7b9a8d6b56941e79b81e015f2a34ac06e3064124''
		]
	},
	HiExampleCommit {
		#id : ''7b9a8d6b56941e79b81e015f2a34ac06e3064124'',
		#comment : ''Merge pull request #151 from takano32/fix-spec-debugger-label\n\n20213-SpecDebugger-label-display-session-name'',
		#parentIds : [
			''dc87c11a1261e3fea7cfde58db2960cd370030b2'',
			''1c7a68e6098d35cfb22ab47dc2c296703e6dc43c''
		]
	},
	HiExampleCommit {
		#id : ''1c7a68e6098d35cfb22ab47dc2c296703e6dc43c'',
		#comment : ''fix SpecDebugger label display session name\n'',
		#parentIds : [
			''dc87c11a1261e3fea7cfde58db2960cd370030b2''
		]
	},
	HiExampleCommit {
		#id : ''dc87c11a1261e3fea7cfde58db2960cd370030b2'',
		#comment : ''EXPORT VERSION 60505\n'',
		#parentIds : [
			''2ab5ef11a830c6aa69d1dc15f65737e16066f299''
		]
	},
	HiExampleCommit {
		#id : ''2ab5ef11a830c6aa69d1dc15f65737e16066f299'',
		#comment : ''EXPORT VERSION 60504\n'',
		#parentIds : [
			''eb7b2c08f0b01419bd8f78fd58d4302ceff69d56''
		]
	},
	HiExampleCommit {
		#id : ''eb7b2c08f0b01419bd8f78fd58d4302ceff69d56'',
		#comment : ''EXPORT VERSION 60503\n'',
		#parentIds : [
			''dce3eefad36b4e0039aace1b08edd75225a1133b''
		]
	},
	HiExampleCommit {
		#id : ''dce3eefad36b4e0039aace1b08edd75225a1133b'',
		#comment : ''EXPORT VERSION 60502\n'',
		#parentIds : [
			''9e320c9701e6c1c0aaf90cfeec3168ed2e15d19e''
		]
	},
	HiExampleCommit {
		#id : ''9e320c9701e6c1c0aaf90cfeec3168ed2e15d19e'',
		#comment : ''Merge pull request #95 from akgrant43/Issue20172\n\n20172-Modify FileReference>>moveTo: to work across file systems / devices'',
		#parentIds : [
			''4a5e2ca1afab643cf867f028694b7dcb808049bc'',
			''4921308280a172e8e6cc82db35f429526b90fa38''
		]
	},
	HiExampleCommit {
		#id : ''4a5e2ca1afab643cf867f028694b7dcb808049bc'',
		#comment : ''Merge pull request #96 from akgrant43/Issue20164a\n\n20164-Cache the default working directory within a session'',
		#parentIds : [
			''38b6f5716c7978d414783a299d76d85cb1f699ba'',
			''e889cdc28c0aa9082c57b93a3ad38e53fd5462a9''
		]
	},
	HiExampleCommit {
		#id : ''e889cdc28c0aa9082c57b93a3ad38e53fd5462a9'',
		#comment : ''Cache the default working directory within a session\n\nDiskStore>>defaultWorkingDirectory currently:\n\n1. Calls a primitive to get the image path\n2. Converts the resulting string to a byte array\n3. Converts the byte array back to a string with ZnCharacterEncoder\n4. Converts the string to a path and gets the parent\n\nevery time it is called.\n\nAs an example of how often it can be called: Using Iceberg to clone an\nempty repository, add a small package and synchronise the repository\ncalled #defaultWorkingDirectory around 500 times.  It is called so many\ntimes because FileSystem>>resolve: leads to FileSystem>>resolvePath:\nwhich resolves the supplied path against the working directory, and\n#resolve: is called from many places.  (The number of calls to\n#defaultWorkingDirectory can vary significantly based on the\nenvironment, but you get the idea).\n\nThe value of defaultWorkingDirectory only changes on startup and when the image is saved to a different location.\n\nCaching defaultWorkingDirectory in a class variable and managing the\ncache through session startup and shutdown gives around a 1,000 times performance improvement in #defaultWorkingDirectory.'',
		#parentIds : [
			''38b6f5716c7978d414783a299d76d85cb1f699ba''
		]
	},
	HiExampleCommit {
		#id : ''4921308280a172e8e6cc82db35f429526b90fa38'',
		#comment : ''Modify FileReference>>moveTo: to work across file systems / devices'',
		#parentIds : [
			''38b6f5716c7978d414783a299d76d85cb1f699ba''
		]
	},
	HiExampleCommit {
		#id : ''38b6f5716c7978d414783a299d76d85cb1f699ba'',
		#comment : ''EXPORT VERSION 60501\n'',
		#parentIds : [
			''bfab8fba9b212adb2f7c136206e45ceb9bb8e692''
		]
	},
	HiExampleCommit {
		#id : ''bfab8fba9b212adb2f7c136206e45ceb9bb8e692'',
		#comment : ''EXPORT VERSION 60500\n'',
		#parentIds : [
			''9ce829e86f978a84fa68df20008a3c1ee52e1fae''
		]
	},
	HiExampleCommit {
		#id : ''9ce829e86f978a84fa68df20008a3c1ee52e1fae'',
		#comment : ''EXPORT VERSION 60499\n'',
		#parentIds : [
			''36fcdd25efbeb7109177330939b44f8bbb38a351''
		]
	},
	HiExampleCommit {
		#id : ''36fcdd25efbeb7109177330939b44f8bbb38a351'',
		#comment : ''EXPORT VERSION 60498\n'',
		#parentIds : [
			''ebbc1c0b1e87eef73829b865fe122057101cbafa''
		]
	},
	HiExampleCommit {
		#id : ''ebbc1c0b1e87eef73829b865fe122057101cbafa'',
		#comment : ''Merge pull request #51 from pharo-project/fixDisplaySize\n\nFix display size'',
		#parentIds : [
			''777bc06fc5bb50bfa14aebd9e8fee1e6f8b95aae'',
			''17754476868a939506299da5d3c8f864320048fe''
		]
	},
	HiExampleCommit {
		#id : ''17754476868a939506299da5d3c8f864320048fe'',
		#comment : ''more compatible print version\n'',
		#parentIds : [
			''19cb7db2a4ef5746a89d6f9c41505779cc892fef''
		]
	},
	HiExampleCommit {
		#id : ''19cb7db2a4ef5746a89d6f9c41505779cc892fef'',
		#comment : ''manual fix of the display size in image header\n'',
		#parentIds : [
			''777bc06fc5bb50bfa14aebd9e8fee1e6f8b95aae''
		]
	},
	HiExampleCommit {
		#id : ''777bc06fc5bb50bfa14aebd9e8fee1e6f8b95aae'',
		#comment : ''EXPORT VERSION 60497\n'',
		#parentIds : [
			''e47ab22022a513a98ccbd3d6682cae7ca22ad2dc''
		]
	},
	HiExampleCommit {
		#id : ''e47ab22022a513a98ccbd3d6682cae7ca22ad2dc'',
		#comment : ''Merge pull request #49 from pavel-krivanek/removeOldSourcesDownload\n\ndownload of V50 sources should not be needed anymore'',
		#parentIds : [
			''7899b4aa841ab77b265c225c051d94d2744b7656'',
			''fbe9bb89c03da7fb65d800aac1cd6f4e7ce6230f''
		]
	},
	HiExampleCommit {
		#id : ''fbe9bb89c03da7fb65d800aac1cd6f4e7ce6230f'',
		#comment : ''download of V50 sources should not be needed anymore\n'',
		#parentIds : [
			''7899b4aa841ab77b265c225c051d94d2744b7656''
		]
	},
	HiExampleCommit {
		#id : ''7899b4aa841ab77b265c225c051d94d2744b7656'',
		#comment : ''EXPORT VERSION 60496\n'',
		#parentIds : [
			''e4674bbfcde3565331ab167e03ba134edbc73dc5''
		]
	},
	HiExampleCommit {
		#id : ''e4674bbfcde3565331ab167e03ba134edbc73dc5'',
		#comment : ''temporarily download old sources file'',
		#parentIds : [
			''e10e38d87dbe292c5647bcc1f45ada9a2c863bc0''
		]
	},
	HiExampleCommit {
		#id : ''e10e38d87dbe292c5647bcc1f45ada9a2c863bc0'',
		#comment : ''EXPORT VERSION 60495\n'',
		#parentIds : [
			''2230cb7e3c3567be224da4d104f095025cc2f807''
		]
	},
	HiExampleCommit {
		#id : ''2230cb7e3c3567be224da4d104f095025cc2f807'',
		#comment : ''EXPORT VERSION 60494\n'',
		#parentIds : [
			''90f20e4a07f7886512448fc2ef804a767f7719cb''
		]
	},
	HiExampleCommit {
		#id : ''90f20e4a07f7886512448fc2ef804a767f7719cb'',
		#comment : ''EXPORT VERSION 60493\n'',
		#parentIds : [
			''3058e85f247a9cb85f094444d7b94f07345ddfd6''
		]
	},
	HiExampleCommit {
		#id : ''3058e85f247a9cb85f094444d7b94f07345ddfd6'',
		#comment : ''EXPORT VERSION 60492\n'',
		#parentIds : [
			''3ae66945278acfd405b65e89d101c7ff6f064f62''
		]
	},
	HiExampleCommit {
		#id : ''3ae66945278acfd405b65e89d101c7ff6f064f62'',
		#comment : ''ignore sources and CONFIG\n'',
		#parentIds : [
			''99664dc2f6d9ad050eb3ff9adacda1c3f460308d''
		]
	},
	HiExampleCommit {
		#id : ''99664dc2f6d9ad050eb3ff9adacda1c3f460308d'',
		#comment : ''EXPORT VERSION 60491\n'',
		#parentIds : [
			''a83b5b89d626f4141fba93849465053f7e7e51de''
		]
	},
	HiExampleCommit {
		#id : ''a83b5b89d626f4141fba93849465053f7e7e51de'',
		#comment : ''fix Iceberg/libgit export\n'',
		#parentIds : [
			''35044a23e23021dee0ee7d609a96411d979cc2c0''
		]
	},
	HiExampleCommit {
		#id : ''35044a23e23021dee0ee7d609a96411d979cc2c0'',
		#comment : ''Merge pull request #47 from pavel-krivanek/icebergPostIntegrationFix\n\n- do not load Iceberg'',
		#parentIds : [
			''cea36f0c8172104867455b3a854252364099c455'',
			''ce86d8d7b2241079b87fcf4cfb4bc3fdd73ca516''
		]
	},
	HiExampleCommit {
		#id : ''ce86d8d7b2241079b87fcf4cfb4bc3fdd73ca516'',
		#comment : ''- do not load Iceberg\n- change repository creation\n'',
		#parentIds : [
			''cea36f0c8172104867455b3a854252364099c455''
		]
	},
	HiExampleCommit {
		#id : ''cea36f0c8172104867455b3a854252364099c455'',
		#comment : ''EXPORT VERSION 60490\n'',
		#parentIds : [
			''75bfe03360d2d78dd29bca1b4795040a76339fec''
		]
	},
	HiExampleCommit {
		#id : ''75bfe03360d2d78dd29bca1b4795040a76339fec'',
		#comment : ''EXPORT VERSION 60489\n'',
		#parentIds : [
			''f2f4cddd8f48415f7fe2e09d76b5af2bed40420e''
		]
	},
	HiExampleCommit {
		#id : ''f2f4cddd8f48415f7fe2e09d76b5af2bed40420e'',
		#comment : ''EXPORT VERSION 60488\n'',
		#parentIds : [
			''21bb3f7792aa69d44264ac90db0445f615d099f9''
		]
	},
	HiExampleCommit {
		#id : ''21bb3f7792aa69d44264ac90db0445f615d099f9'',
		#comment : ''EXPORT VERSION 60487\n'',
		#parentIds : [
			''4ab2cc0dcbaf81984f74d7f75b7a3b0e4542c3e5''
		]
	},
	HiExampleCommit {
		#id : ''4ab2cc0dcbaf81984f74d7f75b7a3b0e4542c3e5'',
		#comment : ''EXPORT VERSION 60486\n'',
		#parentIds : [
			''e9d735bb490b6099127a5c62feadc2082f83f7c3''
		]
	},
	HiExampleCommit {
		#id : ''e9d735bb490b6099127a5c62feadc2082f83f7c3'',
		#comment : ''EXPORT VERSION 60485\n'',
		#parentIds : [
			''80530698b442ad8052ec4f9922de909542244c4d''
		]
	},
	HiExampleCommit {
		#id : ''80530698b442ad8052ec4f9922de909542244c4d'',
		#comment : ''EXPORT VERSION 60484\n'',
		#parentIds : [
			''7c2a845c6e687558a0043c83fcb56fa9261cf076''
		]
	},
	HiExampleCommit {
		#id : ''7c2a845c6e687558a0043c83fcb56fa9261cf076'',
		#comment : ''Merge pull request #46 from guillep/issue45\n\nadd cleanup for release at the end'',
		#parentIds : [
			''4d92279ee46ff3f67eaefbdd5272c09a4cd3e600'',
			''3d82e209f5b1f4d7a641c54c3044168fac097265''
		]
	},
	HiExampleCommit {
		#id : ''3d82e209f5b1f4d7a641c54c3044168fac097265'',
		#comment : ''add cleanup for release at the end\n'',
		#parentIds : [
			''4d92279ee46ff3f67eaefbdd5272c09a4cd3e600''
		]
	},
	HiExampleCommit {
		#id : ''4d92279ee46ff3f67eaefbdd5272c09a4cd3e600'',
		#comment : ''EXPORT VERSION 60483\n'',
		#parentIds : [
			''350a3cbe3e90a4dd188b0b3fa3054e4bdc5e1f68''
		]
	},
	HiExampleCommit {
		#id : ''350a3cbe3e90a4dd188b0b3fa3054e4bdc5e1f68'',
		#comment : ''EXPORT VERSION 60482\n'',
		#parentIds : [
			''b273f92848321e252c6e0c6b764769fe3a9fe61f''
		]
	},
	HiExampleCommit {
		#id : ''b273f92848321e252c6e0c6b764769fe3a9fe61f'',
		#comment : ''EXPORT VERSION 60481\n'',
		#parentIds : [
			''0d0976bc07117c7099a0a26ea0fc0f4178afd229''
		]
	},
	HiExampleCommit {
		#id : ''0d0976bc07117c7099a0a26ea0fc0f4178afd229'',
		#comment : ''EXPORT VERSION 60480\n'',
		#parentIds : [
			''900451f37fdf32428d95cb337d9c8651403a19bf''
		]
	},
	HiExampleCommit {
		#id : ''900451f37fdf32428d95cb337d9c8651403a19bf'',
		#comment : ''EXPORT VERSION 60479\n'',
		#parentIds : [
			''3016c3b9eac3ed298c76b39bff14e83ae7e81835''
		]
	},
	HiExampleCommit {
		#id : ''3016c3b9eac3ed298c76b39bff14e83ae7e81835'',
		#comment : ''EXPORT VERSION 60477\n'',
		#parentIds : [
			''906c3e740b523f4adcb832274288ec920966d182''
		]
	},
	HiExampleCommit {
		#id : ''906c3e740b523f4adcb832274288ec920966d182'',
		#comment : ''EXPORT VERSION 60476\n'',
		#parentIds : [
			''cfc1aa3c84d8ea7a10da4dab061b8c1469242f50''
		]
	},
	HiExampleCommit {
		#id : ''cfc1aa3c84d8ea7a10da4dab061b8c1469242f50'',
		#comment : ''EXPORT VERSION 60475\n'',
		#parentIds : [
			''e4a19bb211951db4b665f636a02f19efd7601d3f''
		]
	},
	HiExampleCommit {
		#id : ''e4a19bb211951db4b665f636a02f19efd7601d3f'',
		#comment : ''EXPORT VERSION 60474\n'',
		#parentIds : [
			''29fb90ba0b06b38483707242051e44f58ae08455''
		]
	},
	HiExampleCommit {
		#id : ''29fb90ba0b06b38483707242051e44f58ae08455'',
		#comment : ''EXPORT VERSION 60473\n'',
		#parentIds : [
			''83de80734c6e112f92aa57a541ade7c1da4975f1''
		]
	},
	HiExampleCommit {
		#id : ''83de80734c6e112f92aa57a541ade7c1da4975f1'',
		#comment : ''EXPORT VERSION 60472\n'',
		#parentIds : [
			''5b70502655da2a3fb3acdd960149aefc9f798130''
		]
	},
	HiExampleCommit {
		#id : ''5b70502655da2a3fb3acdd960149aefc9f798130'',
		#comment : ''EXPORT VERSION 60471\n'',
		#parentIds : [
			''c037987413703d32fc38a389ae20278d42fe4f9e''
		]
	},
	HiExampleCommit {
		#id : ''c037987413703d32fc38a389ae20278d42fe4f9e'',
		#comment : ''EXPORT VERSION 60470\n'',
		#parentIds : [
			''ea7416aa97ed020df374ac49739d87b938b1f481''
		]
	},
	HiExampleCommit {
		#id : ''ea7416aa97ed020df374ac49739d87b938b1f481'',
		#comment : ''EXPORT VERSION 60469\n'',
		#parentIds : [
			''281b7edb334829ed03f5ca8f05c2cb76811d010d''
		]
	},
	HiExampleCommit {
		#id : ''281b7edb334829ed03f5ca8f05c2cb76811d010d'',
		#comment : ''fix message\n'',
		#parentIds : [
			''ebb00f9e5d97e86ac10cb6a558e5d950fffe65ba''
		]
	},
	HiExampleCommit {
		#id : ''ebb00f9e5d97e86ac10cb6a558e5d950fffe65ba'',
		#comment : ''EXPORT VERSION 60468\n'',
		#parentIds : [
			''35691730c2bff822a35275dac3e0a03e11bf384b''
		]
	},
	HiExampleCommit {
		#id : ''35691730c2bff822a35275dac3e0a03e11bf384b'',
		#comment : ''fixing latest export\n'',
		#parentIds : [
			''5e6096c5196c43266c0e7b1cf5677a20edb8ad98''
		]
	},
	HiExampleCommit {
		#id : ''5e6096c5196c43266c0e7b1cf5677a20edb8ad98'',
		#comment : ''removing redirection to /dev/null\n'',
		#parentIds : [
			''2f395c217b4711d68a7926e51f8123c6a179d1fe''
		]
	},
	HiExampleCommit {
		#id : ''2f395c217b4711d68a7926e51f8123c6a179d1fe'',
		#comment : ''excluding bootstrap from pull requests\n'',
		#parentIds : [
			''dfee5ab07cfd16b4c0824f6599a5d05e76d27bce''
		]
	},
	HiExampleCommit {
		#id : ''dfee5ab07cfd16b4c0824f6599a5d05e76d27bce'',
		#comment : ''allow test failures\n'',
		#parentIds : [
			''437109a0ebd8d883baa8be5091f77d2baf4f40b1''
		]
	},
	HiExampleCommit {
		#id : ''437109a0ebd8d883baa8be5091f77d2baf4f40b1'',
		#comment : ''testing bootstrapped image and not bootstrapping image\n'',
		#parentIds : [
			''018c791658574785b4cd5c4909c9e5584a56876d''
		]
	},
	HiExampleCommit {
		#id : ''018c791658574785b4cd5c4909c9e5584a56876d'',
		#comment : ''sending build output to /dev/null to see if it is faster\n'',
		#parentIds : [
			''87afa441d0690f2b8a252245593e40f4d02352a8''
		]
	},
	HiExampleCommit {
		#id : ''87afa441d0690f2b8a252245593e40f4d02352a8'',
		#comment : ''declare temp vars to remove warnings\n'',
		#parentIds : [
			''e52cf439955960fa9bdc42394f75d53a94124e4f''
		]
	},
	HiExampleCommit {
		#id : ''e52cf439955960fa9bdc42394f75d53a94124e4f'',
		#comment : ''removing warnings by declaring temp vars\n'',
		#parentIds : [
			''4a4f6b16ae0d2bf940f948dce3adf689efdaae78''
		]
	},
	HiExampleCommit {
		#id : ''4a4f6b16ae0d2bf940f948dce3adf689efdaae78'',
		#comment : ''remove mac builds, they are sloow\n'',
		#parentIds : [
			''dabe4e94babcbbdc7f1fd085bfc4a50d05db53c6''
		]
	},
	HiExampleCommit {
		#id : ''dabe4e94babcbbdc7f1fd085bfc4a50d05db53c6'',
		#comment : ''added test run after bootstrap\n'',
		#parentIds : [
			''3be1e512131cba389e6eabfeafc068d11884e742''
		]
	},
	HiExampleCommit {
		#id : ''3be1e512131cba389e6eabfeafc068d11884e742'',
		#comment : ''fixed caching tab to space\n'',
		#parentIds : [
			''4b90c2b4018ab0a36756b776c24abe3e775b1256''
		]
	},
	HiExampleCommit {
		#id : ''4b90c2b4018ab0a36756b776c24abe3e775b1256'',
		#comment : ''adding caching of metacello and pharo directories\n'',
		#parentIds : [
			''567e381edf50eccecd3a785b4f508a6ede901845''
		]
	},
	HiExampleCommit {
		#id : ''567e381edf50eccecd3a785b4f508a6ede901845'',
		#comment : ''Merge branch \''master\'' of https://github.com/guillep/pharo-core\n'',
		#parentIds : [
			''97dbbb3b16118f458dbd7f4d010521533154419f'',
			''b4383f1db1f47ac096b9e7b8cef0793717fb35d5''
		]
	},
	HiExampleCommit {
		#id : ''97dbbb3b16118f458dbd7f4d010521533154419f'',
		#comment : ''reduce git depth\n'',
		#parentIds : [
			''fa0a9733f895a619e5b38e4589a5edca779515f5''
		]
	},
	HiExampleCommit {
		#id : ''b4383f1db1f47ac096b9e7b8cef0793717fb35d5'',
		#comment : ''EXPORT VERSION 60467\n'',
		#parentIds : [
			''534336fd0b5c2cdf6734be4c3847db67907e1ca9''
		]
	},
	HiExampleCommit {
		#id : ''534336fd0b5c2cdf6734be4c3847db67907e1ca9'',
		#comment : ''EXPORT VERSION 60466\n'',
		#parentIds : [
			''7482c1476c14b1bb80a9a5447afa003c03383bda''
		]
	},
	HiExampleCommit {
		#id : ''7482c1476c14b1bb80a9a5447afa003c03383bda'',
		#comment : ''deprecate old export script\n'',
		#parentIds : [
			''04dcc3180a60c7c69691b784d8465c67a8afa1fd''
		]
	},
	HiExampleCommit {
		#id : ''04dcc3180a60c7c69691b784d8465c67a8afa1fd'',
		#comment : ''new export script\n'',
		#parentIds : [
			''f3642ab3507c30bcd8d22f5c11b7ac4984e6a69c''
		]
	},
	HiExampleCommit {
		#id : ''f3642ab3507c30bcd8d22f5c11b7ac4984e6a69c'',
		#comment : ''EXPORT VERSION 60465\n'',
		#parentIds : [
			''fa0a9733f895a619e5b38e4589a5edca779515f5''
		]
	},
	HiExampleCommit {
		#id : ''fa0a9733f895a619e5b38e4589a5edca779515f5'',
		#comment : ''put pharo version in env\n'',
		#parentIds : [
			''da1ad02d8ec18b2b41bbd1096ba7d8d440e54749''
		]
	},
	HiExampleCommit {
		#id : ''da1ad02d8ec18b2b41bbd1096ba7d8d440e54749'',
		#comment : ''restored pharo version\n'',
		#parentIds : [
			''aac6b54bd36e9c0667731ae884f1aabafaec375d''
		]
	},
	HiExampleCommit {
		#id : ''aac6b54bd36e9c0667731ae884f1aabafaec375d'',
		#comment : ''fixed bash if statement -> missing space\n'',
		#parentIds : [
			''96378a59d309be075e89003064c434b0b841d6f3''
		]
	},
	HiExampleCommit {
		#id : ''96378a59d309be075e89003064c434b0b841d6f3'',
		#comment : ''removed extra os matrix\n'',
		#parentIds : [
			''387e5023c9899549f71fc58d5f3f9320bccd8356''
		]
	},
	HiExampleCommit {
		#id : ''387e5023c9899549f71fc58d5f3f9320bccd8356'',
		#comment : ''test for all platforms, bootstrap only in linux, publish only bootstrap\n'',
		#parentIds : [
			''9b3be249084d4d5386e0a2752968cbbb4150d1ab''
		]
	},
	HiExampleCommit {
		#id : ''9b3be249084d4d5386e0a2752968cbbb4150d1ab'',
		#comment : ''make it run on linux and mac, make it download the correct vm given the platform\n'',
		#parentIds : [
			''0d747dcba387bd639985877ab7b07c00ff0040cb''
		]
	},
	HiExampleCommit {
		#id : ''0d747dcba387bd639985877ab7b07c00ff0040cb'',
		#comment : ''EXPORT VERSION 60464\n'',
		#parentIds : [
			''3bdab453d070ce03db893c5eccc30ba676c06d5e''
		]
	},
	HiExampleCommit {
		#id : ''3bdab453d070ce03db893c5eccc30ba676c06d5e'',
		#comment : ''EXPORT VERSION 60463\n'',
		#parentIds : [
			''820bfcfcb945d6fea82d6d235d565e593278c906''
		]
	},
	HiExampleCommit {
		#id : ''820bfcfcb945d6fea82d6d235d565e593278c906'',
		#comment : ''EXPORT VERSION 60462\n'',
		#parentIds : [
			''0259a8d265fa8053ea5cec02b4f39e2267c46990''
		]
	},
	HiExampleCommit {
		#id : ''0259a8d265fa8053ea5cec02b4f39e2267c46990'',
		#comment : ''EXPORT VERSION 60460\n'',
		#parentIds : [
			''813f85ef79967386e7728cab6e607b31252ebe9a''
		]
	},
	HiExampleCommit {
		#id : ''813f85ef79967386e7728cab6e607b31252ebe9a'',
		#comment : ''EXPORT VERSION 60459\n'',
		#parentIds : [
			''ca9608c3a5bd3999eec38465c5b8a36fbba9f0f8''
		]
	},
	HiExampleCommit {
		#id : ''ca9608c3a5bd3999eec38465c5b8a36fbba9f0f8'',
		#comment : ''EXPORT VERSION 60458\n'',
		#parentIds : [
			''32e25b085c7f3ec5259e5ecb0ccbb6d6c2c522cd''
		]
	},
	HiExampleCommit {
		#id : ''32e25b085c7f3ec5259e5ecb0ccbb6d6c2c522cd'',
		#comment : ''EXPORT VERSION 60457\n'',
		#parentIds : [
			''43d0e88dd6e71f95d35f8425447ebb04c7170a6b''
		]
	},
	HiExampleCommit {
		#id : ''43d0e88dd6e71f95d35f8425447ebb04c7170a6b'',
		#comment : ''EXPORT VERSION 60456\n'',
		#parentIds : [
			''97fc0d6b06aa109f33de9bc0d0517181831e60e2''
		]
	},
	HiExampleCommit {
		#id : ''97fc0d6b06aa109f33de9bc0d0517181831e60e2'',
		#comment : ''EXPORT VERSION 60455\n'',
		#parentIds : [
			''62be6ea470a7aafe88ca52341c07aeb9ca3f118e''
		]
	},
	HiExampleCommit {
		#id : ''62be6ea470a7aafe88ca52341c07aeb9ca3f118e'',
		#comment : ''EXPORT VERSION 60454\n'',
		#parentIds : [
			''bd82f4fe9f66e21818b10ea2f705564414b73d09''
		]
	},
	HiExampleCommit {
		#id : ''bd82f4fe9f66e21818b10ea2f705564414b73d09'',
		#comment : ''EXPORT VERSION 60453\n'',
		#parentIds : [
			''38a3adeb9fee727fdfb3f38bf29e846b6dd35841''
		]
	},
	HiExampleCommit {
		#id : ''38a3adeb9fee727fdfb3f38bf29e846b6dd35841'',
		#comment : ''EXPORT VERSION 60452\n'',
		#parentIds : [
			''fb1f1dd924d9fe15986a53f487d6604c4c6428d2''
		]
	},
	HiExampleCommit {
		#id : ''fb1f1dd924d9fe15986a53f487d6604c4c6428d2'',
		#comment : ''EXPORT VERSION 60451\n'',
		#parentIds : [
			''281ce22b17d8755b347abeddab906c4a1c9a45f5''
		]
	},
	HiExampleCommit {
		#id : ''281ce22b17d8755b347abeddab906c4a1c9a45f5'',
		#comment : ''EXPORT VERSION 60450\n'',
		#parentIds : [
			''cec9b3d2f9f2c7ef1e803209b8011ac21cfb3879''
		]
	},
	HiExampleCommit {
		#id : ''cec9b3d2f9f2c7ef1e803209b8011ac21cfb3879'',
		#comment : ''EXPORT VERSION 60449\n'',
		#parentIds : [
			''bf9af750760336c7818dfbca65c3df0c87cfa80f''
		]
	},
	HiExampleCommit {
		#id : ''bf9af750760336c7818dfbca65c3df0c87cfa80f'',
		#comment : ''EXPORT VERSION 60448\n'',
		#parentIds : [
			''ce2a99466f47da210f5f16262f41d2615bd6c3d5''
		]
	},
	HiExampleCommit {
		#id : ''ce2a99466f47da210f5f16262f41d2615bd6c3d5'',
		#comment : ''EXPORT VERSION 60447\n'',
		#parentIds : [
			''aa135edb236e607a620c513a6db0c1339fea27bf''
		]
	},
	HiExampleCommit {
		#id : ''aa135edb236e607a620c513a6db0c1339fea27bf'',
		#comment : ''EXPORT VERSION 60446\n'',
		#parentIds : [
			''a1bc5de9fce0afe4cfe799ef1074f3b96736c27a''
		]
	},
	HiExampleCommit {
		#id : ''a1bc5de9fce0afe4cfe799ef1074f3b96736c27a'',
		#comment : ''EXPORT VERSION 60445\n'',
		#parentIds : [
			''4fcf45e8bbc45905683598ae81392e8ef1c03bf0''
		]
	},
	HiExampleCommit {
		#id : ''4fcf45e8bbc45905683598ae81392e8ef1c03bf0'',
		#comment : ''EXPORT VERSION 60444\n'',
		#parentIds : [
			''dca79dcb38a14256f944148a92b2abfbc1941d40''
		]
	},
	HiExampleCommit {
		#id : ''dca79dcb38a14256f944148a92b2abfbc1941d40'',
		#comment : ''EXPORT VERSION 60443\n'',
		#parentIds : [
			''e3cebcdf266c19479c2677ffa3251e1253cc3a85''
		]
	},
	HiExampleCommit {
		#id : ''e3cebcdf266c19479c2677ffa3251e1253cc3a85'',
		#comment : ''EXPORT VERSION 60442\n'',
		#parentIds : [
			''429d34498f0b68f31dfc9592c65b5e18c888faac''
		]
	},
	HiExampleCommit {
		#id : ''429d34498f0b68f31dfc9592c65b5e18c888faac'',
		#comment : ''EXPORT VERSION 60441\n'',
		#parentIds : [
			''10bf54be3bbb745baa815101fe8c8aa04de66a62''
		]
	},
	HiExampleCommit {
		#id : ''10bf54be3bbb745baa815101fe8c8aa04de66a62'',
		#comment : ''Merge pull request #34 from guillep/issue15\n\nuse standalone version of IceMetadatalessFileTreeWriter'',
		#parentIds : [
			''e5494cd47e5a644f1e0ef17e0f2c11f84d511f46'',
			''f05fe155ada2a6c55442ad57d793be5ba93aa17d''
		]
	},
	HiExampleCommit {
		#id : ''f05fe155ada2a6c55442ad57d793be5ba93aa17d'',
		#comment : ''use standalone version of IceMetadatalessFileTreeWriter\n'',
		#parentIds : [
			''e5494cd47e5a644f1e0ef17e0f2c11f84d511f46''
		]
	},
	HiExampleCommit {
		#id : ''e5494cd47e5a644f1e0ef17e0f2c11f84d511f46'',
		#comment : ''EXPORT VERSION 60440\n'',
		#parentIds : [
			''9946db6b6803ae8d3887143a5965edf5ee20bbc2''
		]
	},
	HiExampleCommit {
		#id : ''9946db6b6803ae8d3887143a5965edf5ee20bbc2'',
		#comment : ''EXPORT VERSION 60439\n'',
		#parentIds : [
			''226f2dd96aab461395595785ca5bddbaeba177df''
		]
	},
	HiExampleCommit {
		#id : ''226f2dd96aab461395595785ca5bddbaeba177df'',
		#comment : ''EXPORT VERSION 60438\n'',
		#parentIds : [
			''b7023722ab1989ceeacad9c52a847832f08b513a''
		]
	},
	HiExampleCommit {
		#id : ''b7023722ab1989ceeacad9c52a847832f08b513a'',
		#comment : ''EXPORT VERSION 60437\n'',
		#parentIds : [
			''574413182edef681828ffd84f614c25b60e29a1d''
		]
	},
	HiExampleCommit {
		#id : ''574413182edef681828ffd84f614c25b60e29a1d'',
		#comment : ''EXPORT VERSION 60436\n'',
		#parentIds : [
			''946a6f30f3d5ba8e7b2ca90f2fdfc94a309505c7''
		]
	},
	HiExampleCommit {
		#id : ''946a6f30f3d5ba8e7b2ca90f2fdfc94a309505c7'',
		#comment : ''EXPORT VERSION 60435\n'',
		#parentIds : [
			''a4623c5283b61c1ba5eae00839c544e1a173441c''
		]
	},
	HiExampleCommit {
		#id : ''a4623c5283b61c1ba5eae00839c544e1a173441c'',
		#comment : ''EXPORT VERSION 60434\n'',
		#parentIds : [
			''87059e4b110ffbf066209f6862d6a00e6f43eb28''
		]
	},
	HiExampleCommit {
		#id : ''87059e4b110ffbf066209f6862d6a00e6f43eb28'',
		#comment : ''EXPORT VERSION 60433\n'',
		#parentIds : [
			''8886dbce639541eb9203642590d4021a02401083''
		]
	},
	HiExampleCommit {
		#id : ''8886dbce639541eb9203642590d4021a02401083'',
		#comment : ''EXPORT VERSION 60432\n'',
		#parentIds : [
			''2096565e256ea883ea263bad5ea7e1ff24ca0426''
		]
	},
	HiExampleCommit {
		#id : ''2096565e256ea883ea263bad5ea7e1ff24ca0426'',
		#comment : ''EXPORT VERSION 60431\n'',
		#parentIds : [
			''6923539de78fe2ede738115fc1d8d707bbef30d7''
		]
	},
	HiExampleCommit {
		#id : ''6923539de78fe2ede738115fc1d8d707bbef30d7'',
		#comment : ''Merge pull request #31 from guillep/issue30\n\nproper access to the class side'',
		#parentIds : [
			''bb20a16ef0b5ec74c48e0ba1c3c23a93c134f1a8'',
			''bc5e9b0d46a005911efe3f663805acb29c5291d5''
		]
	},
	HiExampleCommit {
		#id : ''bc5e9b0d46a005911efe3f663805acb29c5291d5'',
		#comment : ''proper access to the class side\n'',
		#parentIds : [
			''bb20a16ef0b5ec74c48e0ba1c3c23a93c134f1a8''
		]
	},
	HiExampleCommit {
		#id : ''bb20a16ef0b5ec74c48e0ba1c3c23a93c134f1a8'',
		#comment : ''EXPORT VERSION 60429\n'',
		#parentIds : [
			''bbcd045c7be905046a18fadfe1412b74fb605447''
		]
	},
	HiExampleCommit {
		#id : ''bbcd045c7be905046a18fadfe1412b74fb605447'',
		#comment : ''EXPORT VERSION 60428\n'',
		#parentIds : [
			''b52c964853dc05cc9db25354fc98a99c4c4e5e2f''
		]
	},
	HiExampleCommit {
		#id : ''b52c964853dc05cc9db25354fc98a99c4c4e5e2f'',
		#comment : ''unsubscribe Breakpoint'',
		#parentIds : [
			''d4af626e9096d7e8c73a25946a843941a52eee95''
		]
	},
	HiExampleCommit {
		#id : ''d4af626e9096d7e8c73a25946a843941a52eee95'',
		#comment : ''EXPORT VERSION 60427\n'',
		#parentIds : [
			''061da7e0b4546a70da8910234c5349d250559aac''
		]
	},
	HiExampleCommit {
		#id : ''061da7e0b4546a70da8910234c5349d250559aac'',
		#comment : ''EXPORT VERSION 60426\n'',
		#parentIds : [
			''cbfafeb558bca455e4d774aa9752ec09a25a35cd''
		]
	},
	HiExampleCommit {
		#id : ''cbfafeb558bca455e4d774aa9752ec09a25a35cd'',
		#comment : ''EXPORT VERSION 60425\n'',
		#parentIds : [
			''820e84fc187dc9088c3b4755e9cfb85550358849''
		]
	},
	HiExampleCommit {
		#id : ''820e84fc187dc9088c3b4755e9cfb85550358849'',
		#comment : ''EXPORT VERSION 60424\n'',
		#parentIds : [
			''af7b39a1edfeae70a9d1016ccdf7d4d1b8e1bcfd''
		]
	},
	HiExampleCommit {
		#id : ''af7b39a1edfeae70a9d1016ccdf7d4d1b8e1bcfd'',
		#comment : ''Merge pull request #29 from guillep/issue28\n\nadd Compression package to RPackage initialization'',
		#parentIds : [
			''a71d85f69a6d18ae2c583ab94a9c7f4e8d2be2e8'',
			''ba41e64527f5d2d74ab5d5e357bc0bd430648dc3''
		]
	},
	HiExampleCommit {
		#id : ''ba41e64527f5d2d74ab5d5e357bc0bd430648dc3'',
		#comment : ''add Compression package to RPackage initialization\n'',
		#parentIds : [
			''a71d85f69a6d18ae2c583ab94a9c7f4e8d2be2e8''
		]
	},
	HiExampleCommit {
		#id : ''a71d85f69a6d18ae2c583ab94a9c7f4e8d2be2e8'',
		#comment : ''EXPORT VERSION 60422\n'',
		#parentIds : [
			''0f5290934fe00cf3e2e64a9ddfb7ca5a6271bc33''
		]
	},
	HiExampleCommit {
		#id : ''0f5290934fe00cf3e2e64a9ddfb7ca5a6271bc33'',
		#comment : ''EXPORT VERSION 60421\n'',
		#parentIds : [
			''bd9d74ba1a9d3171b90222ac53479d60be93cf3d''
		]
	},
	HiExampleCommit {
		#id : ''bd9d74ba1a9d3171b90222ac53479d60be93cf3d'',
		#comment : ''EXPORT VERSION 60420\n'',
		#parentIds : [
			''a9c769064ab67c38f1b098fa4f070835856d2433''
		]
	},
	HiExampleCommit {
		#id : ''a9c769064ab67c38f1b098fa4f070835856d2433'',
		#comment : ''EXPORT VERSION 60419\n'',
		#parentIds : [
			''37ffb732405c9fcec84a144e394171f65d5f846b''
		]
	},
	HiExampleCommit {
		#id : ''37ffb732405c9fcec84a144e394171f65d5f846b'',
		#comment : ''EXPORT VERSION 60418\n'',
		#parentIds : [
			''8e89202ed4c27771649e9b2bb59551a760c13a9d''
		]
	},
	HiExampleCommit {
		#id : ''8e89202ed4c27771649e9b2bb59551a760c13a9d'',
		#comment : ''EXPORT VERSION 60417\n'',
		#parentIds : [
			''7b7f69cbcc79cee371a357ba4ecee42353e1abfb''
		]
	},
	HiExampleCommit {
		#id : ''7b7f69cbcc79cee371a357ba4ecee42353e1abfb'',
		#comment : ''EXPORT VERSION 60416\n'',
		#parentIds : [
			''a932cc600c2101571f369f5ca0ed6d287408ad35''
		]
	},
	HiExampleCommit {
		#id : ''a932cc600c2101571f369f5ca0ed6d287408ad35'',
		#comment : ''EXPORT VERSION 60415\n'',
		#parentIds : [
			''9d38d8554373f0a848df3ee443ac33994134a436''
		]
	},
	HiExampleCommit {
		#id : ''9d38d8554373f0a848df3ee443ac33994134a436'',
		#comment : ''EXPORT VERSION 60414\n'',
		#parentIds : [
			''1de7423a9eab6d9c1c9295dd98c49b34cb57c604''
		]
	},
	HiExampleCommit {
		#id : ''1de7423a9eab6d9c1c9295dd98c49b34cb57c604'',
		#comment : ''EXPORT VERSION 60413\n'',
		#parentIds : [
			''bd6e4a918ad74b9523bc1177accf0aa76ab9b298''
		]
	},
	HiExampleCommit {
		#id : ''bd6e4a918ad74b9523bc1177accf0aa76ab9b298'',
		#comment : ''EXPORT VERSION 60412\n'',
		#parentIds : [
			''c177f852f33d80aec3a65e24ba2241c7454deaf7''
		]
	},
	HiExampleCommit {
		#id : ''c177f852f33d80aec3a65e24ba2241c7454deaf7'',
		#comment : ''EXPORT VERSION 60411\n'',
		#parentIds : [
			''961c6952e7d5ee872341e224ff86c706f3b880fd''
		]
	},
	HiExampleCommit {
		#id : ''961c6952e7d5ee872341e224ff86c706f3b880fd'',
		#comment : ''EXPORT VERSION 60410\n'',
		#parentIds : [
			''1561eb00f15717a92cf8b78c8502e3ba164c1bd8''
		]
	},
	HiExampleCommit {
		#id : ''1561eb00f15717a92cf8b78c8502e3ba164c1bd8'',
		#comment : ''EXPORT VERSION 60409\n'',
		#parentIds : [
			''7a00e23b354180578adc4cfc98f795dc6f89c7ca''
		]
	},
	HiExampleCommit {
		#id : ''7a00e23b354180578adc4cfc98f795dc6f89c7ca'',
		#comment : ''styling'',
		#parentIds : [
			''95209d640a0d02c2a08b3942954f1334b52fe4c9''
		]
	},
	HiExampleCommit {
		#id : ''95209d640a0d02c2a08b3942954f1334b52fe4c9'',
		#comment : ''Merge pull request #26 from guillep/issue24\n\nremove PharoBootstrap-Initialization special export'',
		#parentIds : [
			''a8c5248b067cab864bb581ca16395e62f64fa838'',
			''0864baf6e319ad885559aab51a5e649aeb4e3a1e''
		]
	},
	HiExampleCommit {
		#id : ''a8c5248b067cab864bb581ca16395e62f64fa838'',
		#comment : ''Merge pull request #25 from guillep/issue23\n\ncreate pharo-core symlink sooner and activate the unicode initialization\r\n\r\nnot related failure of 64-bit build'',
		#parentIds : [
			''880dfcedbf8ba4c3fb8afe90c2c06abe7735471a'',
			''78c8cb64e9c4b410b6bbf8d46243c05449ffe245''
		]
	},
	HiExampleCommit {
		#id : ''0864baf6e319ad885559aab51a5e649aeb4e3a1e'',
		#comment : ''remove PharoBootstrap-Initialization special export\n'',
		#parentIds : [
			''880dfcedbf8ba4c3fb8afe90c2c06abe7735471a''
		]
	},
	HiExampleCommit {
		#id : ''78c8cb64e9c4b410b6bbf8d46243c05449ffe245'',
		#comment : ''create pharo-core symlink sooner and activate the unicode initialization\n'',
		#parentIds : [
			''880dfcedbf8ba4c3fb8afe90c2c06abe7735471a''
		]
	},
	HiExampleCommit {
		#id : ''880dfcedbf8ba4c3fb8afe90c2c06abe7735471a'',
		#comment : ''EXPORT VERSION 60408\n'',
		#parentIds : [
			''a5ac80ca9a72fa165999fc9f6bec0bf9b45950b3''
		]
	},
	HiExampleCommit {
		#id : ''a5ac80ca9a72fa165999fc9f6bec0bf9b45950b3'',
		#comment : ''revert initUnicode'',
		#parentIds : [
			''d2554d3b24e8cab2986c1a96881ee158a5cf39f8''
		]
	},
	HiExampleCommit {
		#id : ''d2554d3b24e8cab2986c1a96881ee158a5cf39f8'',
		#comment : ''initialize CombinedChar classPool with the Unicode tables\n'',
		#parentIds : [
			''3b84c395f7b2f2a9b25996220eeaaeed618c3706''
		]
	},
	HiExampleCommit {
		#id : ''3b84c395f7b2f2a9b25996220eeaaeed618c3706'',
		#comment : ''EXPORT VERSION 60407\n'',
		#parentIds : [
			''22e1975ae76559b46cfc8520497bdd43cffc5a3c''
		]
	},
	HiExampleCommit {
		#id : ''22e1975ae76559b46cfc8520497bdd43cffc5a3c'',
		#comment : ''EXPORT VERSION 60406\n'',
		#parentIds : [
			''9f0ff3621662d46ca2d20d53864b55124c1e04e5''
		]
	},
	HiExampleCommit {
		#id : ''9f0ff3621662d46ca2d20d53864b55124c1e04e5'',
		#comment : ''EXPORT VERSION 60405\n'',
		#parentIds : [
			''77f3ea21241c916efc3a4431ad3b5b67fbeed667''
		]
	},
	HiExampleCommit {
		#id : ''77f3ea21241c916efc3a4431ad3b5b67fbeed667'',
		#comment : ''EXPORT VERSION 60404\n'',
		#parentIds : [
			''726941b5dd69a78c644d6399aa9c9cf7bae8b198''
		]
	},
	HiExampleCommit {
		#id : ''726941b5dd69a78c644d6399aa9c9cf7bae8b198'',
		#comment : ''EXPORT VERSION 60401\n'',
		#parentIds : [
			''133546c102b2520da997c8391939870e2b8e5696''
		]
	},
	HiExampleCommit {
		#id : ''133546c102b2520da997c8391939870e2b8e5696'',
		#comment : ''add Unicode data\n'',
		#parentIds : [
			''14a4e25e9b82e9c9935d8840aee7e9dd4f404f18''
		]
	},
	HiExampleCommit {
		#id : ''14a4e25e9b82e9c9935d8840aee7e9dd4f404f18'',
		#comment : ''EXPORT VERSION 60400\n'',
		#parentIds : [
			''49d7ac5068b4e6b6fd73ae870984551a680d580f''
		]
	},
	HiExampleCommit {
		#id : ''49d7ac5068b4e6b6fd73ae870984551a680d580f'',
		#comment : ''EXPORT VERSION 60399\n'',
		#parentIds : [
			''7d706b5ff3d564f48038481f6bfa4eea37f99120''
		]
	},
	HiExampleCommit {
		#id : ''7d706b5ff3d564f48038481f6bfa4eea37f99120'',
		#comment : ''EXPORT VERSION 60398\n'',
		#parentIds : [
			''f47e439ac6d76fcb757283e14af2b51563254259''
		]
	},
	HiExampleCommit {
		#id : ''f47e439ac6d76fcb757283e14af2b51563254259'',
		#comment : ''EXPORT VERSION 60397\n'',
		#parentIds : [
			''b4196deb4b86f77a6e11b33be5edf27b63487043''
		]
	},
	HiExampleCommit {
		#id : ''b4196deb4b86f77a6e11b33be5edf27b63487043'',
		#comment : ''Merge pull request #21 from guillep/issue20\n\nremove already resolved overrides'',
		#parentIds : [
			''8b609953fc3f0a4ba132022b144fedcbee7ad95b'',
			''64349b8fb347b57b7ba78d0b49c7ed95d1bcaafc''
		]
	},
	HiExampleCommit {
		#id : ''8b609953fc3f0a4ba132022b144fedcbee7ad95b'',
		#comment : ''EXPORT VERSION 60396\n'',
		#parentIds : [
			''fe344b9c9fa86157e4204dabb715330527441ba6''
		]
	},
	HiExampleCommit {
		#id : ''64349b8fb347b57b7ba78d0b49c7ed95d1bcaafc'',
		#comment : ''remove already resolved overrides\n'',
		#parentIds : [
			''fe344b9c9fa86157e4204dabb715330527441ba6''
		]
	},
	HiExampleCommit {
		#id : ''fe344b9c9fa86157e4204dabb715330527441ba6'',
		#comment : ''EXPORT VERSION 60395\n'',
		#parentIds : [
			''2b59a8a6834056fdbdbe7a9ffc5addfd5c1c9507''
		]
	},
	HiExampleCommit {
		#id : ''2b59a8a6834056fdbdbe7a9ffc5addfd5c1c9507'',
		#comment : ''EXPORT VERSION 60394\n'',
		#parentIds : [
			''98b33cfd70074b7bf47d96d0c5538fc64f05e489''
		]
	},
	HiExampleCommit {
		#id : ''98b33cfd70074b7bf47d96d0c5538fc64f05e489'',
		#comment : ''EXPORT VERSION 60393\n'',
		#parentIds : [
			''dc9abaef8f50dfa845194592c9975b6ff1e0f67b''
		]
	},
	HiExampleCommit {
		#id : ''dc9abaef8f50dfa845194592c9975b6ff1e0f67b'',
		#comment : ''EXPORT VERSION 60392\n'',
		#parentIds : [
			''2982b949ffdcb8084489a0595944beff50c29f6c''
		]
	},
	HiExampleCommit {
		#id : ''2982b949ffdcb8084489a0595944beff50c29f6c'',
		#comment : ''EXPORT VERSION 60391\n'',
		#parentIds : [
			''a91e8733361d908fdcf99e63805af780939e374e''
		]
	},
	HiExampleCommit {
		#id : ''a91e8733361d908fdcf99e63805af780939e374e'',
		#comment : ''EXPORT VERSION 60390\n'',
		#parentIds : [
			''d49932a33cdd1976a7d41edec88b94528c4595be''
		]
	},
	HiExampleCommit {
		#id : ''d49932a33cdd1976a7d41edec88b94528c4595be'',
		#comment : ''EXPORT VERSION 60389\n'',
		#parentIds : [
			''fe505bfe03e7c77c4577e0fd1e5b0ce8840bcdca''
		]
	},
	HiExampleCommit {
		#id : ''fe505bfe03e7c77c4577e0fd1e5b0ce8840bcdca'',
		#comment : ''EXPORT VERSION 60388\n'',
		#parentIds : [
			''7e07e70b4311464cce31cda58c69cd4993b876c1''
		]
	},
	HiExampleCommit {
		#id : ''7e07e70b4311464cce31cda58c69cd4993b876c1'',
		#comment : ''EXPORT VERSION 60387\n'',
		#parentIds : [
			''be1efc68eb508d9a7c1cc07707e66a5b46c4607e''
		]
	},
	HiExampleCommit {
		#id : ''be1efc68eb508d9a7c1cc07707e66a5b46c4607e'',
		#comment : ''Added description of the artifacts'',
		#parentIds : [
			''cecc08d3a11e1e10fba92f9399a22133ef96cc04''
		]
	},
	HiExampleCommit {
		#id : ''cecc08d3a11e1e10fba92f9399a22133ef96cc04'',
		#comment : ''EXPORT VERSION 60386\n'',
		#parentIds : [
			''02850c07ef67ad4c6abbc54394cfa4af7f00e232''
		]
	},
	HiExampleCommit {
		#id : ''02850c07ef67ad4c6abbc54394cfa4af7f00e232'',
		#comment : ''EXPORT VERSION 60385\n'',
		#parentIds : [
			''693d47ecda5c16417155620f730fe64d8f62daa1''
		]
	},
	HiExampleCommit {
		#id : ''693d47ecda5c16417155620f730fe64d8f62daa1'',
		#comment : ''EXPORT VERSION 60384\n'',
		#parentIds : [
			''48afc0e4395df2a9d1ce26ec7156a39edfbe0cb4''
		]
	},
	HiExampleCommit {
		#id : ''48afc0e4395df2a9d1ce26ec7156a39edfbe0cb4'',
		#comment : ''EXPORT VERSION 60383\n'',
		#parentIds : [
			''e376ff59a1044325a5451b49d299c1d7289b30f7''
		]
	},
	HiExampleCommit {
		#id : ''e376ff59a1044325a5451b49d299c1d7289b30f7'',
		#comment : ''EXPORT VERSION 60382\n'',
		#parentIds : [
			''2f2f9deaf379d1219fefee01e2ec06e9d0cda825''
		]
	},
	HiExampleCommit {
		#id : ''2f2f9deaf379d1219fefee01e2ec06e9d0cda825'',
		#comment : ''EXPORT VERSION 60381\n'',
		#parentIds : [
			''cc2b04b588b59e41b2d7b3e95f19ae57f9064a51''
		]
	},
	HiExampleCommit {
		#id : ''cc2b04b588b59e41b2d7b3e95f19ae57f9064a51'',
		#comment : ''Exit export on error\n\nFix for issue #16. Set flag errexit to exit on error. Also fix shebang for portability.'',
		#parentIds : [
			''0341e08281c9795fab65ba561abb180e16ff899b''
		]
	},
	HiExampleCommit {
		#id : ''0341e08281c9795fab65ba561abb180e16ff899b'',
		#comment : ''use dev-0.4-pre-multi-remotes branch of Iceberg'',
		#parentIds : [
			''6ab83cb6b2d12e96ab6215e9af94ad287830495b''
		]
	},
	HiExampleCommit {
		#id : ''6ab83cb6b2d12e96ab6215e9af94ad287830495b'',
		#comment : ''fixed baseline of pharo bootstrap process to not conflict loading epicea/ston\n'',
		#parentIds : [
			''a1967e1c35ab867efab622a4628fcc4ebce0d48f''
		]
	},
	HiExampleCommit {
		#id : ''a1967e1c35ab867efab622a4628fcc4ebce0d48f'',
		#comment : ''EXPORT VERSION 60362\n'',
		#parentIds : [
			''53f2582580920d33a20c46b8fbb84f0785940de9''
		]
	},
	HiExampleCommit {
		#id : ''53f2582580920d33a20c46b8fbb84f0785940de9'',
		#comment : ''EXPORT VERSION 60361\n'',
		#parentIds : [
			''bb7222e12a1761f933910601ec1ac52b91d314eb''
		]
	},
	HiExampleCommit {
		#id : ''bb7222e12a1761f933910601ec1ac52b91d314eb'',
		#comment : ''EXPORT VERSION 60360\n'',
		#parentIds : [
			''3aadcf4d21973727aa06bf54142c81b5ca311989''
		]
	},
	HiExampleCommit {
		#id : ''3aadcf4d21973727aa06bf54142c81b5ca311989'',
		#comment : ''EXPORT VERSION 60359\n'',
		#parentIds : [
			''4917881ce4186aefc0040bd24de5450cbde84b70''
		]
	},
	HiExampleCommit {
		#id : ''4917881ce4186aefc0040bd24de5450cbde84b70'',
		#comment : ''EXPORT VERSION 60358\n'',
		#parentIds : [
			''c03caf5018d4fafa345c15ce251a699109df69c1''
		]
	},
	HiExampleCommit {
		#id : ''c03caf5018d4fafa345c15ce251a699109df69c1'',
		#comment : ''EXPORT VERSION 60357\n'',
		#parentIds : [
			''6a9faf121ad3121beb4ab696d5cc492425ff0b86''
		]
	},
	HiExampleCommit {
		#id : ''6a9faf121ad3121beb4ab696d5cc492425ff0b86'',
		#comment : ''EXPORT VERSION 60356\n'',
		#parentIds : [
			''65995545a3eeca9f1689ae0ba1ce100f8d5ed499''
		]
	},
	HiExampleCommit {
		#id : ''65995545a3eeca9f1689ae0ba1ce100f8d5ed499'',
		#comment : ''EXPORT VERSION 60355\n'',
		#parentIds : [
			''b761799e9bda8302c7bf638c293ade438fbf741e''
		]
	},
	HiExampleCommit {
		#id : ''b761799e9bda8302c7bf638c293ade438fbf741e'',
		#comment : ''EXPORT VERSION 60354\n'',
		#parentIds : [
			''15e7b3f6ec37a09890c99b414d82015f4c4d00d2''
		]
	},
	HiExampleCommit {
		#id : ''15e7b3f6ec37a09890c99b414d82015f4c4d00d2'',
		#comment : ''EXPORT VERSION 60353\n'',
		#parentIds : [
			''8196ec02b441b382e81df925b1b2d81858d096cd''
		]
	},
	HiExampleCommit {
		#id : ''8196ec02b441b382e81df925b1b2d81858d096cd'',
		#comment : ''EXPORT VERSION 60352\n'',
		#parentIds : [
			''b0e92affe00d29751609bd5b7a17be1fe153631f''
		]
	},
	HiExampleCommit {
		#id : ''b0e92affe00d29751609bd5b7a17be1fe153631f'',
		#comment : ''Merge pull request #12 from guillep/ThemeIcons\n\nThemeIcons initialization and pre-downloading of icons'',
		#parentIds : [
			''52651209f9fc0d73993c1c1421780c2de1c30ef5'',
			''95d368fdad0c2fda64ab97af8ef3f5df1d781cee''
		]
	},
	HiExampleCommit {
		#id : ''52651209f9fc0d73993c1c1421780c2de1c30ef5'',
		#comment : ''EXPORT VERSION 60351\n'',
		#parentIds : [
			''1dfede0b1f3e36b8ad7e96e5afd703e24d5a07de''
		]
	},
	HiExampleCommit {
		#id : ''95d368fdad0c2fda64ab97af8ef3f5df1d781cee'',
		#comment : ''ThemeIcons initialization and pre-downloading of icons\n'',
		#parentIds : [
			''1dfede0b1f3e36b8ad7e96e5afd703e24d5a07de''
		]
	},
	HiExampleCommit {
		#id : ''1dfede0b1f3e36b8ad7e96e5afd703e24d5a07de'',
		#comment : ''EXPORT VERSION 60350\n'',
		#parentIds : [
			''dc3e2e3c48158678a69670928312b7628e9053c5''
		]
	},
	HiExampleCommit {
		#id : ''dc3e2e3c48158678a69670928312b7628e9053c5'',
		#comment : ''EXPORT VERSION 60349\n'',
		#parentIds : [
			''a4732d9624d1d4c229452774e39d35ec4300c4dd''
		]
	},
	HiExampleCommit {
		#id : ''a4732d9624d1d4c229452774e39d35ec4300c4dd'',
		#comment : ''EXPORT VERSION 60348\n'',
		#parentIds : [
			''975bd47f2068f558b41bbe74df202047461e4677''
		]
	},
	HiExampleCommit {
		#id : ''975bd47f2068f558b41bbe74df202047461e4677'',
		#comment : ''EXPORT VERSION 60348\n'',
		#parentIds : [
			''9ab09f0f61c30fcca2831c54630f9e70a20b5bd7''
		]
	},
	HiExampleCommit {
		#id : ''9ab09f0f61c30fcca2831c54630f9e70a20b5bd7'',
		#comment : ''EXPORT VERSION 60347\n'',
		#parentIds : [
			''95ad7b5327205d841c86d23b6223b44ab7ae4bac''
		]
	},
	HiExampleCommit {
		#id : ''95ad7b5327205d841c86d23b6223b44ab7ae4bac'',
		#comment : ''EXPORT VERSION 60346\n'',
		#parentIds : [
			''7ea74f098351381fdb24c4b156f31ffecd00e817''
		]
	},
	HiExampleCommit {
		#id : ''7ea74f098351381fdb24c4b156f31ffecd00e817'',
		#comment : ''EXPORT VERSION 60345\n'',
		#parentIds : [
			''178bc27bca19f68fb6cf3a1ad7be0c9c7559398f''
		]
	},
	HiExampleCommit {
		#id : ''178bc27bca19f68fb6cf3a1ad7be0c9c7559398f'',
		#comment : ''removed printing of protocols - people can check the file\n'',
		#parentIds : [
			''0ae9239658868bade1c4068d8e0345d65992b650''
		]
	},
	HiExampleCommit {
		#id : ''0ae9239658868bade1c4068d8e0345d65992b650'',
		#comment : ''fixed unzip command\n'',
		#parentIds : [
			''615fafe40ef378da5d5e3900e97cb718a281e43d''
		]
	},
	HiExampleCommit {
		#id : ''615fafe40ef378da5d5e3900e97cb718a281e43d'',
		#comment : ''removing sudo permissions\n'',
		#parentIds : [
			''d34acaa33b61f8b4c17b72b2a9ba2914cdc791ea''
		]
	},
	HiExampleCommit {
		#id : ''d34acaa33b61f8b4c17b72b2a9ba2914cdc791ea'',
		#comment : ''added new download buttons\n'',
		#parentIds : [
			''9b50942b720fbb0ba001410b51435e048fbcb289''
		]
	},
	HiExampleCommit {
		#id : ''9b50942b720fbb0ba001410b51435e048fbcb289'',
		#comment : ''fixed save flag in last step of build\n'',
		#parentIds : [
			''78004910fb0483491e81dada685ac1c49afe17f6''
		]
	},
	HiExampleCommit {
		#id : ''78004910fb0483491e81dada685ac1c49afe17f6'',
		#comment : ''saving ide image\n'',
		#parentIds : [
			''f69a5967d33130fd6d07d2249a62f53b092de7c6''
		]
	},
	HiExampleCommit {
		#id : ''f69a5967d33130fd6d07d2249a62f53b092de7c6'',
		#comment : ''Merge branch \''master\'' of github.com:guillep/pharo-core\n'',
		#parentIds : [
			''9c39b49a37bdfd5d7a98ab7791428fd0809e928d'',
			''c9e22ebef80042e3c136a6e434a2d10ce7358015''
		]
	},
	HiExampleCommit {
		#id : ''9c39b49a37bdfd5d7a98ab7791428fd0809e928d'',
		#comment : ''deploying in different bintray projects\n'',
		#parentIds : [
			''cf5714d86ae985fb7960b298721a059b40e5c8b8''
		]
	},
	HiExampleCommit {
		#id : ''c9e22ebef80042e3c136a6e434a2d10ce7358015'',
		#comment : ''Added bintray download information\n\nFix issue #2'',
		#parentIds : [
			''cf5714d86ae985fb7960b298721a059b40e5c8b8''
		]
	},
	HiExampleCommit {
		#id : ''cf5714d86ae985fb7960b298721a059b40e5c8b8'',
		#comment : ''merge with 64bit vm branch\n'',
		#parentIds : [
			''aa0d7e2216631db873a9fbd46eee441f17bc3598'',
			''7ba0455f265fa63d59b3bf19f265286846413353''
		]
	},
	HiExampleCommit {
		#id : ''aa0d7e2216631db873a9fbd46eee441f17bc3598'',
		#comment : ''downloading sources with VM\n'',
		#parentIds : [
			''4c57563f9b6c9a34a537e0c91041eedad4696c10''
		]
	},
	HiExampleCommit {
		#id : ''7ba0455f265fa63d59b3bf19f265286846413353'',
		#comment : ''Updated build instructions\n\nFixed issue #9'',
		#parentIds : [
			''24e9f3457c9d068499d6a0d93b510f9ac8e37e51''
		]
	},
	HiExampleCommit {
		#id : ''24e9f3457c9d068499d6a0d93b510f9ac8e37e51'',
		#comment : ''Updated dependencies\n\n- PharoBootstrapProcess should be loaded from local filetree directory\r\n- Download latest iceberg'',
		#parentIds : [
			''26b3d7a44f51f41fd984be9b503cb31d40a636c4''
		]
	},
	HiExampleCommit {
		#id : ''4c57563f9b6c9a34a537e0c91041eedad4696c10'',
		#comment : ''Add sudo permissions to execute threaded VM'',
		#parentIds : [
			''cdf701060f6cc0be1adeb95c15a1185856ba32d7''
		]
	},
	HiExampleCommit {
		#id : ''cdf701060f6cc0be1adeb95c15a1185856ba32d7'',
		#comment : ''Sudoing threaded VM\n\nThis is required to launch the VM in all travis slave'',
		#parentIds : [
			''1bedb1fa5c0ab3be7c86073fde76792bfdf8cf41''
		]
	},
	HiExampleCommit {
		#id : ''1bedb1fa5c0ab3be7c86073fde76792bfdf8cf41'',
		#comment : ''Added vm display null'',
		#parentIds : [
			''d703cb1f3c36eac6c880c7676a6fbcdc27f4ed84''
		]
	},
	HiExampleCommit {
		#id : ''26b3d7a44f51f41fd984be9b503cb31d40a636c4'',
		#comment : ''- bootstrap emulation scripts\n'',
		#parentIds : [
			''d666df73d45cc042161d449ee0b6a44f9c0615af''
		]
	},
	HiExampleCommit {
		#id : ''d666df73d45cc042161d449ee0b6a44f9c0615af'',
		#comment : ''Using headless vm in build script'',
		#parentIds : [
			''86a7a338b191bebbd3e3ec2aa035a50b0e637cae''
		]
	},
	HiExampleCommit {
		#id : ''86a7a338b191bebbd3e3ec2aa035a50b0e637cae'',
		#comment : ''EXPORT VERSION 60344\n'',
		#parentIds : [
			''3292e03abb91f28f567aef1dad4fd3e8780fb4a5''
		]
	},
	HiExampleCommit {
		#id : ''3292e03abb91f28f567aef1dad4fd3e8780fb4a5'',
		#comment : ''add info.st\n'',
		#parentIds : [
			''2ab9744d3b4bfc4bc590907b3db4e1589be423b1''
		]
	},
	HiExampleCommit {
		#id : ''d703cb1f3c36eac6c880c7676a6fbcdc27f4ed84'',
		#comment : ''downloading the correct VM given the architecture\n'',
		#parentIds : [
			''2ab9744d3b4bfc4bc590907b3db4e1589be423b1''
		]
	},
	HiExampleCommit {
		#id : ''2ab9744d3b4bfc4bc590907b3db4e1589be423b1'',
		#comment : ''fix issue #3: architecture argument was wrongly passed to smalltalk script\n'',
		#parentIds : [
			''4efa2227e9b42b9a2ae1ef706b0bb8c61be73805''
		]
	},
	HiExampleCommit {
		#id : ''4efa2227e9b42b9a2ae1ef706b0bb8c61be73805'',
		#comment : ''merge with fixgttools branch\n'',
		#parentIds : [
			''81993478b0c9b0efab219d16e25d8caa1dee550b'',
			''064bfe16ca67660059140465998ed2795899af22''
		]
	},
	HiExampleCommit {
		#id : ''81993478b0c9b0efab219d16e25d8caa1dee550b'',
		#comment : ''EXPORT VERSION 60343\n'',
		#parentIds : [
			''03a69427492d990c466dba8c10df54fc602f390f''
		]
	},
	HiExampleCommit {
		#id : ''03a69427492d990c466dba8c10df54fc602f390f'',
		#comment : ''EXPORT VERSION 60342\n'',
		#parentIds : [
			''10a05c42e5c1dbefc6141ad4dd67770154723af3''
		]
	},
	HiExampleCommit {
		#id : ''10a05c42e5c1dbefc6141ad4dd67770154723af3'',
		#comment : ''EXPORT VERSION 60341\n'',
		#parentIds : [
			''79dadef6e1d5ad5de9244de6d00f7ecda5541f41''
		]
	},
	HiExampleCommit {
		#id : ''064bfe16ca67660059140465998ed2795899af22'',
		#comment : ''added Glamour-SpecIntegration package in baseline\n'',
		#parentIds : [
			''79dadef6e1d5ad5de9244de6d00f7ecda5541f41''
		]
	},
	HiExampleCommit {
		#id : ''79dadef6e1d5ad5de9244de6d00f7ecda5541f41'',
		#comment : ''EXPORT VERSION 60340\n'',
		#parentIds : [
			''0c3515796ff5b441d1beadb74ee5a0296f2bbed9''
		]
	},
	HiExampleCommit {
		#id : ''0c3515796ff5b441d1beadb74ee5a0296f2bbed9'',
		#comment : ''EXPORT VERSION 60339\n'',
		#parentIds : [
			''c1ef252778407fc2c9d77fb934c638133e9a20d2''
		]
	},
	HiExampleCommit {
		#id : ''c1ef252778407fc2c9d77fb934c638133e9a20d2'',
		#comment : ''Merge branch \''bintray-integration\'' of github.com:guillep/pharo-core\n'',
		#parentIds : [
			''914fde9de77dffa98f8c17c940bae5435b5beaae'',
			''6dc9f132b1644332098c2779de8893aa198650a4''
		]
	},
	HiExampleCommit {
		#id : ''6dc9f132b1644332098c2779de8893aa198650a4'',
		#comment : ''enhanced build script with optional arguments and prefixing build artifacts\n'',
		#parentIds : [
			''6ce2de747862d4d8e3c743f1d17a21c37808b852''
		]
	},
	HiExampleCommit {
		#id : ''6ce2de747862d4d8e3c743f1d17a21c37808b852'',
		#comment : ''added check as after deploy\n'',
		#parentIds : [
			''57346d6917e3a0304ce074963b3a7ad5c161c1ff''
		]
	},
	HiExampleCommit {
		#id : ''57346d6917e3a0304ce074963b3a7ad5c161c1ff'',
		#comment : ''added deploy check script\n'',
		#parentIds : [
			''d9f9eb8bde697be88165fe27a72cdc1efd86cb8f''
		]
	},
	HiExampleCommit {
		#id : ''d9f9eb8bde697be88165fe27a72cdc1efd86cb8f'',
		#comment : ''fixed include pattern to publish all zips\n'',
		#parentIds : [
			''95fa32311b04fd0e046a3eec27c56112de5dc5b4''
		]
	},
	HiExampleCommit {
		#id : ''914fde9de77dffa98f8c17c940bae5435b5beaae'',
		#comment : ''EXPORT VERSION 60338\n'',
		#parentIds : [
			''74f2a256c732cdcc37f01ee0ea9b07c3ac3dca79''
		]
	},
	HiExampleCommit {
		#id : ''74f2a256c732cdcc37f01ee0ea9b07c3ac3dca79'',
		#comment : ''EXPORT VERSION 60337\n'',
		#parentIds : [
			''1bc1a9830c15eae960619e1ef77518e07a8c49c9''
		]
	},
	HiExampleCommit {
		#id : ''1bc1a9830c15eae960619e1ef77518e07a8c49c9'',
		#comment : ''EXPORT VERSION 60336\n'',
		#parentIds : [
			''499d8a0767300f23279e3e7a8dba1c0ae3b21734''
		]
	},
	HiExampleCommit {
		#id : ''499d8a0767300f23279e3e7a8dba1c0ae3b21734'',
		#comment : ''EXPORT VERSION 60335\n'',
		#parentIds : [
			''dfbfb77844a31701da6e49691af2a05ad55b4bc6''
		]
	},
	HiExampleCommit {
		#id : ''95fa32311b04fd0e046a3eec27c56112de5dc5b4'',
		#comment : ''remove date from bintray request. Current date does not match expected format\n'',
		#parentIds : [
			''100e8f5abf903de8dae4d8260ce7f21c6cc4599b''
		]
	},
	HiExampleCommit {
		#id : ''100e8f5abf903de8dae4d8260ce7f21c6cc4599b'',
		#comment : ''added architecture in name of the version\n'',
		#parentIds : [
			''d0715e0c454d61137d1be583860f6e2c3d8f5afa''
		]
	},
	HiExampleCommit {
		#id : ''d0715e0c454d61137d1be583860f6e2c3d8f5afa'',
		#comment : ''fixed permisions and version names in perl script\n'',
		#parentIds : [
			''c9982d4910b35d5fbca7016cca59c37d39b7eb82''
		]
	},
	HiExampleCommit {
		#id : ''c9982d4910b35d5fbca7016cca59c37d39b7eb82'',
		#comment : ''added bintray integration\n'',
		#parentIds : [
			''dfbfb77844a31701da6e49691af2a05ad55b4bc6''
		]
	},
	HiExampleCommit {
		#id : ''dfbfb77844a31701da6e49691af2a05ad55b4bc6'',
		#comment : ''removed rms that fail in travis. Make assumption of empty world\n'',
		#parentIds : [
			''329b07e381cf6289327a8195dead1151fe796503''
		]
	},
	HiExampleCommit {
		#id : ''329b07e381cf6289327a8195dead1151fe796503'',
		#comment : ''restored prepare image script\n'',
		#parentIds : [
			''147b32e7125bf4805dea0aa9039cb2e0d0317ad9''
		]
	},
	HiExampleCommit {
		#id : ''147b32e7125bf4805dea0aa9039cb2e0d0317ad9'',
		#comment : ''Merge branch \''master\'' of github.com:guillep/pharo-core into monticellobootstrap\n'',
		#parentIds : [
			''b87a4450e1150d1fc5ac584f4c04c6cd594dcea2'',
			''957ee2bc867287166da41039db6358fcf96d484a''
		]
	},
	HiExampleCommit {
		#id : ''b87a4450e1150d1fc5ac584f4c04c6cd594dcea2'',
		#comment : ''integrating full image build into ci\n'',
		#parentIds : [
			''839b7d4e3bb8254187f7a242b6c65dc590b956dc''
		]
	},
	HiExampleCommit {
		#id : ''839b7d4e3bb8254187f7a242b6c65dc590b956dc'',
		#comment : ''enhanced build script\n'',
		#parentIds : [
			''ff2c5ada5797791307cc4d7a0cc5145dea49d954''
		]
	},
	HiExampleCommit {
		#id : ''ff2c5ada5797791307cc4d7a0cc5145dea49d954'',
		#comment : ''splitted image preparation from bootstrap itself\n'',
		#parentIds : [
			''32ed8a698aec44651ce2aecbe3c1c9cc9957b704''
		]
	},
	HiExampleCommit {
		#id : ''957ee2bc867287166da41039db6358fcf96d484a'',
		#comment : ''EXPORT VERSION 60334\n'',
		#parentIds : [
			''a3711b26845a841d1d0a7344b001aea9931052a8''
		]
	},
	HiExampleCommit {
		#id : ''32ed8a698aec44651ce2aecbe3c1c9cc9957b704'',
		#comment : ''Manual merge properties.json\n'',
		#parentIds : [
			''1e1a24a5dcf65f7fae2e8f67601025b35b929606'',
			''a3711b26845a841d1d0a7344b001aea9931052a8''
		]
	},
	HiExampleCommit {
		#id : ''1e1a24a5dcf65f7fae2e8f67601025b35b929606'',
		#comment : ''fixed typo in initialize method\n'',
		#parentIds : [
			''43bce78c0041f1b2561780c4d0c0125b3078870b''
		]
	},
	HiExampleCommit {
		#id : ''a3711b26845a841d1d0a7344b001aea9931052a8'',
		#comment : ''EXPORT VERSION 60333\n'',
		#parentIds : [
			''f8048067c4eed731c62edd02774cb602f9740f08''
		]
	},
	HiExampleCommit {
		#id : ''43bce78c0041f1b2561780c4d0c0125b3078870b'',
		#comment : ''Merge branch \''master\'' of github.com:guillep/pharo-core into monticellobootstrap\n'',
		#parentIds : [
			''17aac3debd2a1423373e095702d171826958dadf'',
			''f8048067c4eed731c62edd02774cb602f9740f08''
		]
	},
	HiExampleCommit {
		#id : ''17aac3debd2a1423373e095702d171826958dadf'',
		#comment : ''removed old filetree metadata\n'',
		#parentIds : [
			''755681aa628cdc906e043360d6fc950beb2d04b2''
		]
	},
	HiExampleCommit {
		#id : ''755681aa628cdc906e043360d6fc950beb2d04b2'',
		#comment : ''fixed bootstrap creation\n'',
		#parentIds : [
			''bb0714dea86c928e31e1e0ac5eddf07e10f97fc2''
		]
	},
	HiExampleCommit {
		#id : ''bb0714dea86c928e31e1e0ac5eddf07e10f97fc2'',
		#comment : ''added build script that builds monticello and metacello\n'',
		#parentIds : [
			''41b908acace6f87810bb0c3f7688c402da1bf632''
		]
	},
	HiExampleCommit {
		#id : ''41b908acace6f87810bb0c3f7688c402da1bf632'',
		#comment : ''fixed bootstrap src filetree repo\n'',
		#parentIds : [
			''2b2ef1587586e13d45435786a149d785930b08ff''
		]
	},
	HiExampleCommit {
		#id : ''2b2ef1587586e13d45435786a149d785930b08ff'',
		#comment : ''moved bootstrap script to bootstrap folder\n'',
		#parentIds : [
			''ad807afc077bb126cb75f877b64916ee1236de59''
		]
	},
	HiExampleCommit {
		#id : ''ad807afc077bb126cb75f877b64916ee1236de59'',
		#comment : ''fixed travis scaping\n'',
		#parentIds : [
			''df2c8e798a6d25bfb117fb3b09c3488d16b18786''
		]
	},
	HiExampleCommit {
		#id : ''df2c8e798a6d25bfb117fb3b09c3488d16b18786'',
		#comment : ''using new bootstrap API\n'',
		#parentIds : [
			''33b332f1bf3758283fb21b1da6133448987ae6bc''
		]
	},
	HiExampleCommit {
		#id : ''33b332f1bf3758283fb21b1da6133448987ae6bc'',
		#comment : ''added new constructore\n'',
		#parentIds : [
			''fb6baacee5ce4682d019b40e4cd0cd017e3eb25d''
		]
	},
	HiExampleCommit {
		#id : ''fb6baacee5ce4682d019b40e4cd0cd017e3eb25d'',
		#comment : ''removed old distribution thingy\n'',
		#parentIds : [
			''5c6fe03355cf9041890ba20042bdc616f50506c2''
		]
	},
	HiExampleCommit {
		#id : ''5c6fe03355cf9041890ba20042bdc616f50506c2'',
		#comment : ''added bootstrap cache directory into ignore\n'',
		#parentIds : [
			''0d6267bce1adfd4e67fe4740350402424f078f96''
		]
	},
	HiExampleCommit {
		#id : ''0d6267bce1adfd4e67fe4740350402424f078f96'',
		#comment : ''make bootstrap image in bootstrap cache directory\n'',
		#parentIds : [
			''7c8e6df688470e39661e8409bba96305d897a2b7''
		]
	},
	HiExampleCommit {
		#id : ''7c8e6df688470e39661e8409bba96305d897a2b7'',
		#comment : ''added metacello bootstrap scripts\n'',
		#parentIds : [
			''ad042098b9a1ed7299e7654b0d1fe1dc16331bb1''
		]
	},
	HiExampleCommit {
		#id : ''f8048067c4eed731c62edd02774cb602f9740f08'',
		#comment : ''EXPORT VERSION 60332\n'',
		#parentIds : [
			''806f9340be8df9b769340bc515d6ca163cabd18a''
		]
	},
	HiExampleCommit {
		#id : ''ad042098b9a1ed7299e7654b0d1fe1dc16331bb1'',
		#comment : ''added dejavu fonts\n'',
		#parentIds : [
			''f1de1811da055e218ea290cef8649694173f3a0c''
		]
	},
	HiExampleCommit {
		#id : ''f1de1811da055e218ea290cef8649694173f3a0c'',
		#comment : ''numerate monticello bootstrap scripts\n'',
		#parentIds : [
			''ee24553efb4a5bac351b903d369907e038c92aa6''
		]
	},
	HiExampleCommit {
		#id : ''ee24553efb4a5bac351b903d369907e038c92aa6'',
		#comment : ''numerate initialization scripts\n'',
		#parentIds : [
			''322178ed51a901b303cfdbd4f985097a7f2e0797''
		]
	},
	HiExampleCommit {
		#id : ''322178ed51a901b303cfdbd4f985097a7f2e0797'',
		#comment : ''extracted bootstrap process from image generation\n'',
		#parentIds : [
			''00a03328a61db26bfdf198869edf4d3f7fe41bd7''
		]
	},
	HiExampleCommit {
		#id : ''00a03328a61db26bfdf198869edf4d3f7fe41bd7'',
		#comment : ''added monticello bootstrap class\n'',
		#parentIds : [
			''74dc7851222b766e09ac3bd0553086a6cef52956''
		]
	},
	HiExampleCommit {
		#id : ''806f9340be8df9b769340bc515d6ca163cabd18a'',
		#comment : ''EXPORT VERSION 60331\n'',
		#parentIds : [
			''95a4c74949a2501046796abe9ffe2ab26533af56''
		]
	},
	HiExampleCommit {
		#id : ''95a4c74949a2501046796abe9ffe2ab26533af56'',
		#comment : ''EXPORT VERSION 60330\n'',
		#parentIds : [
			''541e58be66c8d4cc546f0f0278a392f80f584ed7''
		]
	},
	HiExampleCommit {
		#id : ''541e58be66c8d4cc546f0f0278a392f80f584ed7'',
		#comment : ''EXPORT VERSION 60329\n'',
		#parentIds : [
			''75258990221c2a717436a0d8644e5174b70d9be1''
		]
	},
	HiExampleCommit {
		#id : ''75258990221c2a717436a0d8644e5174b70d9be1'',
		#comment : ''EXPORT VERSION 60328\n'',
		#parentIds : [
			''6e1b77c9fd333226ce36aea0b3dd0aa157a74103''
		]
	},
	HiExampleCommit {
		#id : ''6e1b77c9fd333226ce36aea0b3dd0aa157a74103'',
		#comment : ''EXPORT VERSION 60327\n'',
		#parentIds : [
			''d5ea9a3866b1d43c63f22e4ce1345c5e9a10016a''
		]
	},
	HiExampleCommit {
		#id : ''d5ea9a3866b1d43c63f22e4ce1345c5e9a10016a'',
		#comment : ''EXPORT VERSION 60326\n'',
		#parentIds : [
			''468e80a4adcb6e9f9206d6257a9623fb24136793''
		]
	},
	HiExampleCommit {
		#id : ''74dc7851222b766e09ac3bd0553086a6cef52956'',
		#comment : ''added bootstrap scripts\n'',
		#parentIds : [
			''e785a718e9421a0b94cfd59b47c7c58fa52d3d9f''
		]
	},
	HiExampleCommit {
		#id : ''e785a718e9421a0b94cfd59b47c7c58fa52d3d9f'',
		#comment : ''moved bootstrap sources to subdirectory\n'',
		#parentIds : [
			''17bf2c6689767465d049a8e1a8a05ccc03b583d6''
		]
	},
	HiExampleCommit {
		#id : ''468e80a4adcb6e9f9206d6257a9623fb24136793'',
		#comment : ''EXPORT VERSION 60325\n'',
		#parentIds : [
			''17bf2c6689767465d049a8e1a8a05ccc03b583d6''
		]
	},
	HiExampleCommit {
		#id : ''17bf2c6689767465d049a8e1a8a05ccc03b583d6'',
		#comment : ''extension methods should also log with beacon\n'',
		#parentIds : [
			''a22e7f5d6a73ea1ebb7a0cd1abab846adecfebbd''
		]
	},
	HiExampleCommit {
		#id : ''a22e7f5d6a73ea1ebb7a0cd1abab846adecfebbd'',
		#comment : ''Fixed logging. Should only log strings and not blocks.\n'',
		#parentIds : [
			''206b4d0ac751e7d16895a8f4664e8131b766811a''
		]
	},
	HiExampleCommit {
		#id : ''206b4d0ac751e7d16895a8f4664e8131b766811a'',
		#comment : ''logging using beacon\n'',
		#parentIds : [
			''fe8d3d4ae849a578a0d7c8801de6bed637b5c071''
		]
	},
	HiExampleCommit {
		#id : ''fe8d3d4ae849a578a0d7c8801de6bed637b5c071'',
		#comment : ''initializing beacon loggers\n'',
		#parentIds : [
			''714c4afcacb64b9937782475dc65566764cffe07''
		]
	},
	HiExampleCommit {
		#id : ''714c4afcacb64b9937782475dc65566764cffe07'',
		#comment : ''added beacon dependency\n'',
		#parentIds : [
			''fbba8b68682f88f063fd000e24540dfe404f60f9''
		]
	},
	HiExampleCommit {
		#id : ''fbba8b68682f88f063fd000e24540dfe404f60f9'',
		#comment : ''moved to pharo60\n'',
		#parentIds : [
			''72d47e21fc905655124af3389d7c2f0c9cd6855b''
		]
	},
	HiExampleCommit {
		#id : ''72d47e21fc905655124af3389d7c2f0c9cd6855b'',
		#comment : ''removed unnecessary logging\n'',
		#parentIds : [
			''165ea8261b3927fafc158e469bf682de117d817b''
		]
	},
	HiExampleCommit {
		#id : ''165ea8261b3927fafc158e469bf682de117d817b'',
		#comment : ''Removed usage of IceLibGigLocalRepository'',
		#parentIds : [
			''ace880e78e188918acb6b073771570dbf30f4c11''
		]
	},
	HiExampleCommit {
		#id : ''ace880e78e188918acb6b073771570dbf30f4c11'',
		#comment : ''EXPORT VERSION 60323\n'',
		#parentIds : [
			''91c485f24d77567c44aa3d80579110b247c2ae4f''
		]
	},
	HiExampleCommit {
		#id : ''91c485f24d77567c44aa3d80579110b247c2ae4f'',
		#comment : ''EXPORT VERSION 60322\n'',
		#parentIds : [
			''fd887621981ee48d1f76b0b91421122cf3c3ab41''
		]
	},
	HiExampleCommit {
		#id : ''fd887621981ee48d1f76b0b91421122cf3c3ab41'',
		#comment : ''EXPORT VERSION 60321\n'',
		#parentIds : [
			''64081aedf4b6b4fe542e76dafe476cda082eacf0''
		]
	},
	HiExampleCommit {
		#id : ''64081aedf4b6b4fe542e76dafe476cda082eacf0'',
		#comment : ''EXPORT VERSION 60320\n'',
		#parentIds : [
			''4320da353d916f8adf3f912ccf75303e0608e4d0''
		]
	},
	HiExampleCommit {
		#id : ''4320da353d916f8adf3f912ccf75303e0608e4d0'',
		#comment : ''added statistics and enhanced performance by removing intermediate checkpoints\n'',
		#parentIds : [
			''146e4636ee3d9e3ae18be31a554b34a9e25bdd77''
		]
	},
	HiExampleCommit {
		#id : ''146e4636ee3d9e3ae18be31a554b34a9e25bdd77'',
		#comment : ''updated iceberg version to use\n'',
		#parentIds : [
			''06391767fd0379f254f84ab1c71e7657b97e4ca5''
		]
	},
	HiExampleCommit {
		#id : ''06391767fd0379f254f84ab1c71e7657b97e4ca5'',
		#comment : ''updated bootstrap script\n'',
		#parentIds : [
			''92ceae92c7f89be77126fb35c757f1500946ebc9''
		]
	},
	HiExampleCommit {
		#id : ''92ceae92c7f89be77126fb35c757f1500946ebc9'',
		#comment : ''baseline of bootstrap process references baseline of pharo bootstrap in same repo\n'',
		#parentIds : [
			''e908570bdbabe681df4c7a93399dcc7af61765c5''
		]
	},
	HiExampleCommit {
		#id : ''e908570bdbabe681df4c7a93399dcc7af61765c5'',
		#comment : ''Merge branch \''master\'' of github.com:guillep/pharo-core\n'',
		#parentIds : [
			''64c9df6c13c9f70d9ce00820dec3e09f0134ab77'',
			''320d5119177e26752b61c9aa7dbde43b8de1e496''
		]
	},
	HiExampleCommit {
		#id : ''64c9df6c13c9f70d9ce00820dec3e09f0134ab77'',
		#comment : ''added bootstrap packages\n'',
		#parentIds : [
			''f289695804b36748e4af613008cf099adde2d9ad''
		]
	},
	HiExampleCommit {
		#id : ''320d5119177e26752b61c9aa7dbde43b8de1e496'',
		#comment : ''EXPORT VERSION 60319\n'',
		#parentIds : [
			''f289695804b36748e4af613008cf099adde2d9ad''
		]
	},
	HiExampleCommit {
		#id : ''f289695804b36748e4af613008cf099adde2d9ad'',
		#comment : ''EXPORT VERSION 60318\n'',
		#parentIds : [
			''194c7ce4af93d87772b174e7e0f4f4e8fc74864e''
		]
	},
	HiExampleCommit {
		#id : ''194c7ce4af93d87772b174e7e0f4f4e8fc74864e'',
		#comment : ''EXPORT VERSION 60317\n'',
		#parentIds : [
			''9f15bfb8e90bce0a0483e9a1f55d62a3f91dcc07''
		]
	},
	HiExampleCommit {
		#id : ''9f15bfb8e90bce0a0483e9a1f55d62a3f91dcc07'',
		#comment : ''EXPORT VERSION 60316\n'',
		#parentIds : [
			''628f489fa25c1c68c7d555869ac4a4acd4f0ba97''
		]
	},
	HiExampleCommit {
		#id : ''628f489fa25c1c68c7d555869ac4a4acd4f0ba97'',
		#comment : ''EXPORT VERSION 60315\n'',
		#parentIds : [
			''1d4e6b4f25247c1b47b0299689b27f545384285e''
		]
	},
	HiExampleCommit {
		#id : ''1d4e6b4f25247c1b47b0299689b27f545384285e'',
		#comment : ''EXPORT VERSION 60314\n'',
		#parentIds : [
			''a2cb61a6a5217e39e9618ba9aee248e274a0472f''
		]
	},
	HiExampleCommit {
		#id : ''a2cb61a6a5217e39e9618ba9aee248e274a0472f'',
		#comment : ''EXPORT VERSION 60313\n'',
		#parentIds : [
			''94e1751c0b6e41de1691d9174ce00dcc6a20bc2a''
		]
	},
	HiExampleCommit {
		#id : ''94e1751c0b6e41de1691d9174ce00dcc6a20bc2a'',
		#comment : ''EXPORT VERSION 60312\n'',
		#parentIds : [
			''95b0ca487f3415420b6680a266304693dac6650c''
		]
	},
	HiExampleCommit {
		#id : ''95b0ca487f3415420b6680a266304693dac6650c'',
		#comment : ''EXPORT VERSION 60311\n'',
		#parentIds : [
			''aebe1d2b0f32af7bb5c83ad5e47f90f61619eb94''
		]
	},
	HiExampleCommit {
		#id : ''aebe1d2b0f32af7bb5c83ad5e47f90f61619eb94'',
		#comment : ''EXPORT VERSION 60310\n'',
		#parentIds : [
			''77a64d53ff4e0f137e215c1f917a6b0f15dfa780''
		]
	},
	HiExampleCommit {
		#id : ''77a64d53ff4e0f137e215c1f917a6b0f15dfa780'',
		#comment : ''EXPORT VERSION 60309\n'',
		#parentIds : [
			''c5c3423cdd5ea64d7fb45a7e7fd01a1ec89da6e0''
		]
	},
	HiExampleCommit {
		#id : ''c5c3423cdd5ea64d7fb45a7e7fd01a1ec89da6e0'',
		#comment : ''EXPORT VERSION 60308\n'',
		#parentIds : [
			''7387a27554d547dd4cdc4e0b971a6a4ba5c961d4''
		]
	},
	HiExampleCommit {
		#id : ''7387a27554d547dd4cdc4e0b971a6a4ba5c961d4'',
		#comment : ''EXPORT VERSION 60307\n'',
		#parentIds : [
			''5d60a64c666cdb6fb4b8cfcb7ae4c8fd2a0346e8''
		]
	},
	HiExampleCommit {
		#id : ''5d60a64c666cdb6fb4b8cfcb7ae4c8fd2a0346e8'',
		#comment : ''EXPORT VERSION 60306\n'',
		#parentIds : [
			''9d00eae9e34482cbbb96df45b003481d47196617''
		]
	},
	HiExampleCommit {
		#id : ''9d00eae9e34482cbbb96df45b003481d47196617'',
		#comment : ''EXPORT VERSION 60305\n'',
		#parentIds : [
			''7d6be81fd5dfdb1d6fad5d56e439637946d57352''
		]
	},
	HiExampleCommit {
		#id : ''7d6be81fd5dfdb1d6fad5d56e439637946d57352'',
		#comment : ''EXPORT VERSION 60304\n'',
		#parentIds : [
			''f59966d5672400b3d82298d67a1d64afc62dc457''
		]
	},
	HiExampleCommit {
		#id : ''f59966d5672400b3d82298d67a1d64afc62dc457'',
		#comment : ''EXPORT VERSION 60303\n'',
		#parentIds : [
			''893907aaf30da5032c201ef49a4322877ee8fb6b''
		]
	},
	HiExampleCommit {
		#id : ''893907aaf30da5032c201ef49a4322877ee8fb6b'',
		#comment : ''add ScriptLoader60 to exported packages because ScriptLoader does not list yourself\n'',
		#parentIds : [
			''2e0a52af60e5bb0c782b6d419fb73e2b3e6c8cea''
		]
	},
	HiExampleCommit {
		#id : ''2e0a52af60e5bb0c782b6d419fb73e2b3e6c8cea'',
		#comment : ''EXPORT VERSION 60303\n'',
		#parentIds : [
			''902fc00c334b423a28aee9a0270b928d06b5c379''
		]
	},
	HiExampleCommit {
		#id : ''902fc00c334b423a28aee9a0270b928d06b5c379'',
		#comment : ''EXPORT VERSION 60302\n'',
		#parentIds : [
			''f1c394971e770a132a32bc6d5d32091bbbdbf32c''
		]
	},
	HiExampleCommit {
		#id : ''f1c394971e770a132a32bc6d5d32091bbbdbf32c'',
		#comment : ''EXPORT VERSION 60300\n'',
		#parentIds : [
			''35df1ad89f75ace3969166b690b469a5c0fcab24''
		]
	},
	HiExampleCommit {
		#id : ''35df1ad89f75ace3969166b690b469a5c0fcab24'',
		#comment : ''EXPORT VERSION 60299\n'',
		#parentIds : [
			''cf0e763acf2ed1e54491009762464fb6d3a3d479''
		]
	},
	HiExampleCommit {
		#id : ''cf0e763acf2ed1e54491009762464fb6d3a3d479'',
		#comment : ''EXPORT VERSION 60297\n'',
		#parentIds : [
			''8a36e49897f6f9cc2513670d683b33a551f7a837''
		]
	},
	HiExampleCommit {
		#id : ''8a36e49897f6f9cc2513670d683b33a551f7a837'',
		#comment : ''EXPORT VERSION 60296\n'',
		#parentIds : [
			''a1dbe512cfaad6f1b1b26e267ef5f0ebc9206496''
		]
	},
	HiExampleCommit {
		#id : ''a1dbe512cfaad6f1b1b26e267ef5f0ebc9206496'',
		#comment : ''EXPORT VERSION 60295\n'',
		#parentIds : [
			''8e40288ed21652e0a8546f9051a75165adde57c0''
		]
	},
	HiExampleCommit {
		#id : ''8e40288ed21652e0a8546f9051a75165adde57c0'',
		#comment : ''EXPORT VERSION 60294\n'',
		#parentIds : [
			''418d877932dd9027b8a8859118f4b963b0adb1d5''
		]
	},
	HiExampleCommit {
		#id : ''418d877932dd9027b8a8859118f4b963b0adb1d5'',
		#comment : ''EXPORT VERSION 60292\n'',
		#parentIds : [
			''35e76c17c5eee696ac8b146d23920bee56e0e277''
		]
	},
	HiExampleCommit {
		#id : ''35e76c17c5eee696ac8b146d23920bee56e0e277'',
		#comment : ''EXPORT VERSION 60290\n'',
		#parentIds : [
			''66666949ceffbd806ff39188548d65797a1e8314''
		]
	},
	HiExampleCommit {
		#id : ''66666949ceffbd806ff39188548d65797a1e8314'',
		#comment : ''EXPORT VERSION 60289\n'',
		#parentIds : [
			''31de6e2eba42d687c944dd0e0ff1246336167dc4''
		]
	},
	HiExampleCommit {
		#id : ''31de6e2eba42d687c944dd0e0ff1246336167dc4'',
		#comment : ''EXPORT VERSION 60288\n'',
		#parentIds : [
			''469f386f65b0ea2e41bacec42651c0bbe05cebdf''
		]
	},
	HiExampleCommit {
		#id : ''469f386f65b0ea2e41bacec42651c0bbe05cebdf'',
		#comment : ''EXPORT VERSION 60287\n'',
		#parentIds : [
			''dba33ce6005c9a519cc54aad43b78b59be623605''
		]
	},
	HiExampleCommit {
		#id : ''dba33ce6005c9a519cc54aad43b78b59be623605'',
		#comment : ''EXPORT VERSION 60286\n'',
		#parentIds : [
			''cbd9c90038dab3d589bebf1b91bea4d622be4785''
		]
	},
	HiExampleCommit {
		#id : ''cbd9c90038dab3d589bebf1b91bea4d622be4785'',
		#comment : ''Update baseline..st\n\ntemporary add Colors'',
		#parentIds : [
			''1898896ca8007fe755990da49d7aaff89f46918d''
		]
	},
	HiExampleCommit {
		#id : ''1898896ca8007fe755990da49d7aaff89f46918d'',
		#comment : ''EXPORT VERSION 60285\n'',
		#parentIds : [
			''fb23019d1631c12951501cc8c25eaa5501a5c29c''
		]
	},
	HiExampleCommit {
		#id : ''fb23019d1631c12951501cc8c25eaa5501a5c29c'',
		#comment : ''EXPORT VERSION 60284\n'',
		#parentIds : [
			''2c198c17c906b89837eb6e7b56df4618ee3e985f''
		]
	},
	HiExampleCommit {
		#id : ''2c198c17c906b89837eb6e7b56df4618ee3e985f'',
		#comment : ''use libGit for Iceberg'',
		#parentIds : [
			''abf352f665a3aecd95e168baa2b20d097fabe995''
		]
	},
	HiExampleCommit {
		#id : ''abf352f665a3aecd95e168baa2b20d097fabe995'',
		#comment : ''fixed typo in bootstrap script: remove escaping $ for bash\n'',
		#parentIds : [
			''a46894b4f47da269021dc3c6835824ad60ceea9f''
		]
	},
	HiExampleCommit {
		#id : ''a46894b4f47da269021dc3c6835824ad60ceea9f'',
		#comment : ''added compatibility method to use iceberg in pharo5\n'',
		#parentIds : [
			''754a934b921d84d9109d9eb04b188db179b1cfcd''
		]
	},
	HiExampleCommit {
		#id : ''754a934b921d84d9109d9eb04b188db179b1cfcd'',
		#comment : ''fixed iceberg backend for non-interactive mode in CI\n'',
		#parentIds : [
			''d5cb930325bde521f87580077534e8ad5360b3d3''
		]
	},
	HiExampleCommit {
		#id : ''d5cb930325bde521f87580077534e8ad5360b3d3'',
		#comment : ''EXPORT VERSION 60259\n'',
		#parentIds : [
			''c989f83dfdeb7716fecbdd68d0fe47f0a10c0678''
		]
	},
	HiExampleCommit {
		#id : ''c989f83dfdeb7716fecbdd68d0fe47f0a10c0678'',
		#comment : ''EXPORT VERSION 60258\n'',
		#parentIds : [
			''7ddfe21631589b803869710e98a6c06b645900ff''
		]
	},
	HiExampleCommit {
		#id : ''7ddfe21631589b803869710e98a6c06b645900ff'',
		#comment : ''EXPORT VERSION 60257\n'',
		#parentIds : [
			''037d1215f5eb6d686efac993012e8e97d89b92ff''
		]
	},
	HiExampleCommit {
		#id : ''037d1215f5eb6d686efac993012e8e97d89b92ff'',
		#comment : ''EXPORT VERSION 60256\n'',
		#parentIds : [
			''d831246a476f5b49090168fc687f32f3cac26145''
		]
	},
	HiExampleCommit {
		#id : ''d831246a476f5b49090168fc687f32f3cac26145'',
		#comment : ''EXPORT VERSION 60255\n'',
		#parentIds : [
			''e5c9a4066fe202d57b57c25e539bedfc914fd6c9''
		]
	},
	HiExampleCommit {
		#id : ''e5c9a4066fe202d57b57c25e539bedfc914fd6c9'',
		#comment : ''EXPORT VERSION 60254\n'',
		#parentIds : [
			''21f40b7ee1f1f21275ad11a638d6fdd5a495fba2''
		]
	},
	HiExampleCommit {
		#id : ''21f40b7ee1f1f21275ad11a638d6fdd5a495fba2'',
		#comment : ''EXPORT VERSION 60253\n'',
		#parentIds : [
			''b94113b6bf9b5f0865c27ebb3e7410032a737ca0''
		]
	},
	HiExampleCommit {
		#id : ''b94113b6bf9b5f0865c27ebb3e7410032a737ca0'',
		#comment : ''EXPORT VERSION 60252\n'',
		#parentIds : [
			''d3d90393e065f7eebe4eb9446114676b8a1f3c22''
		]
	},
	HiExampleCommit {
		#id : ''d3d90393e065f7eebe4eb9446114676b8a1f3c22'',
		#comment : ''EXPORT VERSION 60251\n'',
		#parentIds : [
			''d8e371d55ab8b3f0ebaeca411e249290579e653a''
		]
	},
	HiExampleCommit {
		#id : ''d8e371d55ab8b3f0ebaeca411e249290579e653a'',
		#comment : ''EXPORT VERSION 60250\n'',
		#parentIds : [
			''b91cc85e22db2b80643bf2b2e303ada810781316''
		]
	},
	HiExampleCommit {
		#id : ''b91cc85e22db2b80643bf2b2e303ada810781316'',
		#comment : ''EXPORT VERSION 60249\n'',
		#parentIds : [
			''08e3f85f3e76a92625a7378d136e2172c29890d5''
		]
	},
	HiExampleCommit {
		#id : ''08e3f85f3e76a92625a7378d136e2172c29890d5'',
		#comment : ''EXPORT VERSION 60248\n'',
		#parentIds : [
			''74d5e1544b5788e0f079cf2eba7f12a7bf1f1ab1''
		]
	},
	HiExampleCommit {
		#id : ''74d5e1544b5788e0f079cf2eba7f12a7bf1f1ab1'',
		#comment : ''EXPORT VERSION 60247\n'',
		#parentIds : [
			''72d6877b55053bd582a643c3655fb69472b1b315''
		]
	},
	HiExampleCommit {
		#id : ''72d6877b55053bd582a643c3655fb69472b1b315'',
		#comment : ''EXPORT VERSION 60246\n'',
		#parentIds : [
			''e1de7fa64550f514020782455897415f4ff35aee''
		]
	},
	HiExampleCommit {
		#id : ''e1de7fa64550f514020782455897415f4ff35aee'',
		#comment : ''EXPORT VERSION 60245\n'',
		#parentIds : [
			''f3e9e28848c7787683134cee29b278ec11c88d74''
		]
	},
	HiExampleCommit {
		#id : ''f3e9e28848c7787683134cee29b278ec11c88d74'',
		#comment : ''EXPORT VERSION 60244\n'',
		#parentIds : [
			''3765d915808e0fc5b63c088045f89547808dafde''
		]
	},
	HiExampleCommit {
		#id : ''3765d915808e0fc5b63c088045f89547808dafde'',
		#comment : ''EXPORT VERSION 60243\n'',
		#parentIds : [
			''c3ad4de6f000e0f409a0af005f2234744266d065''
		]
	},
	HiExampleCommit {
		#id : ''c3ad4de6f000e0f409a0af005f2234744266d065'',
		#comment : ''EXPORT VERSION 60242\n'',
		#parentIds : [
			''232b21d5f3f7176801ba26c390f5996f676a53a8''
		]
	},
	HiExampleCommit {
		#id : ''232b21d5f3f7176801ba26c390f5996f676a53a8'',
		#comment : ''EXPORT VERSION 60241\n'',
		#parentIds : [
			''de8ed27c6b7c659c316e5906de410e64d2347318''
		]
	},
	HiExampleCommit {
		#id : ''de8ed27c6b7c659c316e5906de410e64d2347318'',
		#comment : ''EXPORT VERSION 60240\n'',
		#parentIds : [
			''c8f9959684003087a57d503a7044ae81a1ebe1c1''
		]
	},
	HiExampleCommit {
		#id : ''c8f9959684003087a57d503a7044ae81a1ebe1c1'',
		#comment : ''EXPORT VERSION 60239\n'',
		#parentIds : [
			''d168610854cf73f6dd6f0828e0b0cb64453cc42a''
		]
	},
	HiExampleCommit {
		#id : ''d168610854cf73f6dd6f0828e0b0cb64453cc42a'',
		#comment : ''EXPORT VERSION 60238\n'',
		#parentIds : [
			''08fdc8d1712f91b2c22d95349cdf73f83eac66e1''
		]
	},
	HiExampleCommit {
		#id : ''08fdc8d1712f91b2c22d95349cdf73f83eac66e1'',
		#comment : ''EXPORT VERSION 60237\n'',
		#parentIds : [
			''d34e8b93cc2a9aac91bd133f21fea1537c74e417''
		]
	},
	HiExampleCommit {
		#id : ''d34e8b93cc2a9aac91bd133f21fea1537c74e417'',
		#comment : ''EXPORT VERSION 60236\n'',
		#parentIds : [
			''0bed4ccbee409c1ef44da60f37f68c6241a2eb01''
		]
	},
	HiExampleCommit {
		#id : ''0bed4ccbee409c1ef44da60f37f68c6241a2eb01'',
		#comment : ''EXPORT VERSION 60235\n'',
		#parentIds : [
			''752ecf54c9c9270062355d7d267282292aac86a3''
		]
	},
	HiExampleCommit {
		#id : ''752ecf54c9c9270062355d7d267282292aac86a3'',
		#comment : ''EXPORT VERSION 60234\n'',
		#parentIds : [
			''c4e8ffd2326d1e5092c7293246cf003c411d88f1''
		]
	},
	HiExampleCommit {
		#id : ''c4e8ffd2326d1e5092c7293246cf003c411d88f1'',
		#comment : ''EXPORT VERSION 60233\n'',
		#parentIds : [
			''83519940538cdde7eca294077c908329ab3d3a49''
		]
	},
	HiExampleCommit {
		#id : ''83519940538cdde7eca294077c908329ab3d3a49'',
		#comment : ''EXPORT VERSION 60232\n'',
		#parentIds : [
			''e1ac40513b60ccd78b54b705961d439aef09fac7''
		]
	},
	HiExampleCommit {
		#id : ''e1ac40513b60ccd78b54b705961d439aef09fac7'',
		#comment : ''EXPORT VERSION 60231\n'',
		#parentIds : [
			''05ca97781464c8ef39a2b67e98196a44f83de095''
		]
	},
	HiExampleCommit {
		#id : ''05ca97781464c8ef39a2b67e98196a44f83de095'',
		#comment : ''EXPORT VERSION 60230\n'',
		#parentIds : [
			''eaac9229fa3a838123fe03a3113b3710fdf6beed''
		]
	},
	HiExampleCommit {
		#id : ''eaac9229fa3a838123fe03a3113b3710fdf6beed'',
		#comment : ''EXPORT VERSION 60229\n'',
		#parentIds : [
			''2d894dc72f247c1b5efab8cf63ff8a51ba38dc47''
		]
	},
	HiExampleCommit {
		#id : ''2d894dc72f247c1b5efab8cf63ff8a51ba38dc47'',
		#comment : ''EXPORT VERSION 60228\n'',
		#parentIds : [
			''5bc63bf9d1a1bcac8d39e6f003f197e8e3c9e2fd''
		]
	},
	HiExampleCommit {
		#id : ''5bc63bf9d1a1bcac8d39e6f003f197e8e3c9e2fd'',
		#comment : ''EXPORT VERSION 60227\n'',
		#parentIds : [
			''0b63bb9b367558ec907ca910251da33c08446d62''
		]
	},
	HiExampleCommit {
		#id : ''0b63bb9b367558ec907ca910251da33c08446d62'',
		#comment : ''EXPORT VERSION 60226\n'',
		#parentIds : [
			''321efef86815a3debdf75ab534d7851996938155''
		]
	},
	HiExampleCommit {
		#id : ''321efef86815a3debdf75ab534d7851996938155'',
		#comment : ''EXPORT VERSION 60225\n'',
		#parentIds : [
			''896e1b79cfc05d3cfff89d09dea3c800e4be0767''
		]
	},
	HiExampleCommit {
		#id : ''896e1b79cfc05d3cfff89d09dea3c800e4be0767'',
		#comment : ''EXPORT VERSION 60224\n'',
		#parentIds : [
			''4ca8f3b65e8e19331486121bad524aa1b7e06645''
		]
	},
	HiExampleCommit {
		#id : ''4ca8f3b65e8e19331486121bad524aa1b7e06645'',
		#comment : ''EXPORT VERSION 60223\n'',
		#parentIds : [
			''43cdf8e56922590368dcfd07ca10e334ac2c27e1''
		]
	},
	HiExampleCommit {
		#id : ''43cdf8e56922590368dcfd07ca10e334ac2c27e1'',
		#comment : ''EXPORT VERSION 60222\n'',
		#parentIds : [
			''b8277f30176f30d5b16846bc6436f8251af35257''
		]
	},
	HiExampleCommit {
		#id : ''b8277f30176f30d5b16846bc6436f8251af35257'',
		#comment : ''EXPORT VERSION 60221\n'',
		#parentIds : [
			''5937047619728ec6367f7ba7f9916ff7722144b2''
		]
	},
	HiExampleCommit {
		#id : ''5937047619728ec6367f7ba7f9916ff7722144b2'',
		#comment : ''EXPORT VERSION 60220\n'',
		#parentIds : [
			''de5355700a04352d5772ba2a185399bd34bbbcc9''
		]
	},
	HiExampleCommit {
		#id : ''de5355700a04352d5772ba2a185399bd34bbbcc9'',
		#comment : ''EXPORT VERSION 60219\n'',
		#parentIds : [
			''bc1ad0f37e4cdabb28790b871fb93e2d42baa6ed''
		]
	},
	HiExampleCommit {
		#id : ''bc1ad0f37e4cdabb28790b871fb93e2d42baa6ed'',
		#comment : ''EXPORT VERSION 60218\n'',
		#parentIds : [
			''df1d5ffb012b75638e06dfe9b70c316f24fe6127''
		]
	},
	HiExampleCommit {
		#id : ''df1d5ffb012b75638e06dfe9b70c316f24fe6127'',
		#comment : ''EXPORT VERSION 60217\n'',
		#parentIds : [
			''3204b0e155527aa313b97c204671e2c348913510''
		]
	},
	HiExampleCommit {
		#id : ''3204b0e155527aa313b97c204671e2c348913510'',
		#comment : ''EXPORT VERSION 60216\n'',
		#parentIds : [
			''c8c8c6322fa0d6da88fdb3d73ecf45fddac51b2f''
		]
	},
	HiExampleCommit {
		#id : ''c8c8c6322fa0d6da88fdb3d73ecf45fddac51b2f'',
		#comment : ''EXPORT VERSION 60215\n'',
		#parentIds : [
			''85554739422ed2ff2729b01014d97a47ada44564''
		]
	},
	HiExampleCommit {
		#id : ''85554739422ed2ff2729b01014d97a47ada44564'',
		#comment : ''EXPORT VERSION 60214\n'',
		#parentIds : [
			''8ae92e888751b99241fc18c3903046e75f22306a''
		]
	},
	HiExampleCommit {
		#id : ''8ae92e888751b99241fc18c3903046e75f22306a'',
		#comment : ''EXPORT VERSION 60213\n'',
		#parentIds : [
			''056804ae44a4a7d7f9b6e8c6777a075dfe73dc3f''
		]
	},
	HiExampleCommit {
		#id : ''056804ae44a4a7d7f9b6e8c6777a075dfe73dc3f'',
		#comment : ''EXPORT VERSION 60212\n'',
		#parentIds : [
			''8003bd0ed496e2d4c731fbfd259cee2db158b662''
		]
	},
	HiExampleCommit {
		#id : ''8003bd0ed496e2d4c731fbfd259cee2db158b662'',
		#comment : ''EXPORT VERSION 60211\n'',
		#parentIds : [
			''95ae2f08fed36d05e9976fe9b1b0328167f6fbfa''
		]
	},
	HiExampleCommit {
		#id : ''95ae2f08fed36d05e9976fe9b1b0328167f6fbfa'',
		#comment : ''EXPORT VERSION 60210\n'',
		#parentIds : [
			''fa448136484c1014e4db88531fc6c3033679f2aa''
		]
	},
	HiExampleCommit {
		#id : ''fa448136484c1014e4db88531fc6c3033679f2aa'',
		#comment : ''EXPORT VERSION 60209\n'',
		#parentIds : [
			''a5f72b6bd3dc125cf2b5a4a03837e5f77c808967''
		]
	},
	HiExampleCommit {
		#id : ''a5f72b6bd3dc125cf2b5a4a03837e5f77c808967'',
		#comment : ''EXPORT VERSION 60208\n'',
		#parentIds : [
			''a6addacd6349f583c8a8ade8965c5adc75558feb''
		]
	},
	HiExampleCommit {
		#id : ''a6addacd6349f583c8a8ade8965c5adc75558feb'',
		#comment : ''EXPORT VERSION 60207\n'',
		#parentIds : [
			''a1695f4a615ee9cb5abafb6b58d42a7dda23e879''
		]
	},
	HiExampleCommit {
		#id : ''a1695f4a615ee9cb5abafb6b58d42a7dda23e879'',
		#comment : ''EXPORT VERSION 60206\n'',
		#parentIds : [
			''ea3a9bd584d86165e7ea96eb14e0297af5411867''
		]
	},
	HiExampleCommit {
		#id : ''ea3a9bd584d86165e7ea96eb14e0297af5411867'',
		#comment : ''EXPORT VERSION 60205\n'',
		#parentIds : [
			''587da637a4e83cafd1bd2c6e9ed832fd537270de''
		]
	},
	HiExampleCommit {
		#id : ''587da637a4e83cafd1bd2c6e9ed832fd537270de'',
		#comment : ''EXPORT VERSION 60204\n'',
		#parentIds : [
			''aff4e0fd5c8dd12dff5922881fbeff27e5c4250c''
		]
	},
	HiExampleCommit {
		#id : ''aff4e0fd5c8dd12dff5922881fbeff27e5c4250c'',
		#comment : ''EXPORT VERSION 60203\n'',
		#parentIds : [
			''e18406a2308293643c80efbb1b23b3f1c2dccec7''
		]
	},
	HiExampleCommit {
		#id : ''e18406a2308293643c80efbb1b23b3f1c2dccec7'',
		#comment : ''EXPORT VERSION 60202\n'',
		#parentIds : [
			''e378020b1661a0539d85a16c16210a48a58a5b9f''
		]
	},
	HiExampleCommit {
		#id : ''e378020b1661a0539d85a16c16210a48a58a5b9f'',
		#comment : ''EXPORT VERSION 60201\n'',
		#parentIds : [
			''68fc2f3b3613365fe4520e1f050392b628eba451''
		]
	},
	HiExampleCommit {
		#id : ''68fc2f3b3613365fe4520e1f050392b628eba451'',
		#comment : ''EXPORT VERSION 60200\n'',
		#parentIds : [
			''a6dc211a191529198e1885370959d2dc3f2be784''
		]
	},
	HiExampleCommit {
		#id : ''a6dc211a191529198e1885370959d2dc3f2be784'',
		#comment : ''EXPORT VERSION 60199\n'',
		#parentIds : [
			''0fd86743d24bc99c2ffd13f9b379710717cd3a5d''
		]
	},
	HiExampleCommit {
		#id : ''0fd86743d24bc99c2ffd13f9b379710717cd3a5d'',
		#comment : ''EXPORT VERSION 60198\n'',
		#parentIds : [
			''b3b070b158e49686aff38c45d69cbf11ca61b334''
		]
	},
	HiExampleCommit {
		#id : ''b3b070b158e49686aff38c45d69cbf11ca61b334'',
		#comment : ''EXPORT VERSION 60197\n'',
		#parentIds : [
			''9ec3c323f60c31c21d0431e9efea3bb02121cc97''
		]
	},
	HiExampleCommit {
		#id : ''9ec3c323f60c31c21d0431e9efea3bb02121cc97'',
		#comment : ''EXPORT VERSION 60196\n'',
		#parentIds : [
			''d540e4f76bc4863bd96dfa6817b9418bf856af16''
		]
	},
	HiExampleCommit {
		#id : ''d540e4f76bc4863bd96dfa6817b9418bf856af16'',
		#comment : ''EXPORT VERSION 60195\n'',
		#parentIds : [
			''a78dc6a1974d499682c16dd003dbd9df26ff3174''
		]
	},
	HiExampleCommit {
		#id : ''a78dc6a1974d499682c16dd003dbd9df26ff3174'',
		#comment : ''EXPORT VERSION 60194\n'',
		#parentIds : [
			''a99029a401ad85e575eeb59c98e252a10f93ce25''
		]
	},
	HiExampleCommit {
		#id : ''a99029a401ad85e575eeb59c98e252a10f93ce25'',
		#comment : ''EXPORT VERSION 60193\n'',
		#parentIds : [
			''dbf1e5a8ebe829cf206954c9191de8b6d4d4859c''
		]
	},
	HiExampleCommit {
		#id : ''dbf1e5a8ebe829cf206954c9191de8b6d4d4859c'',
		#comment : ''EXPORT VERSION 60192\n'',
		#parentIds : [
			''dbd00cfbf06f5a42f19553d82db0184d81019f18''
		]
	},
	HiExampleCommit {
		#id : ''dbd00cfbf06f5a42f19553d82db0184d81019f18'',
		#comment : ''EXPORT VERSION 60191\n'',
		#parentIds : [
			''11cc12badc6c6d03ca063cde09edef2aa7fc9fc7''
		]
	},
	HiExampleCommit {
		#id : ''11cc12badc6c6d03ca063cde09edef2aa7fc9fc7'',
		#comment : ''EXPORT VERSION 60190\n'',
		#parentIds : [
			''071a50823050ac3f9c5822bd6966cf41c2a489fe''
		]
	},
	HiExampleCommit {
		#id : ''071a50823050ac3f9c5822bd6966cf41c2a489fe'',
		#comment : ''EXPORT VERSION 60189\n'',
		#parentIds : [
			''67ed0bc902b409be49919423734889dfb688f13c''
		]
	},
	HiExampleCommit {
		#id : ''67ed0bc902b409be49919423734889dfb688f13c'',
		#comment : ''EXPORT VERSION 60188\n'',
		#parentIds : [
			''2db5a4ffe0515dc6d750f615739fdd8c92ef8d49''
		]
	},
	HiExampleCommit {
		#id : ''2db5a4ffe0515dc6d750f615739fdd8c92ef8d49'',
		#comment : ''EXPORT VERSION 60187\n'',
		#parentIds : [
			''39d013b7722837b6672b782ad418a35c5fdb0384''
		]
	},
	HiExampleCommit {
		#id : ''39d013b7722837b6672b782ad418a35c5fdb0384'',
		#comment : ''EXPORT VERSION 60186\n'',
		#parentIds : [
			''2c410b33849799980dce62dc913cda1e4806886c''
		]
	},
	HiExampleCommit {
		#id : ''2c410b33849799980dce62dc913cda1e4806886c'',
		#comment : ''EXPORT VERSION 60185\n'',
		#parentIds : [
			''ff7e23e1ff65b9d2614e8968339a3d8ba9825c5c''
		]
	},
	HiExampleCommit {
		#id : ''ff7e23e1ff65b9d2614e8968339a3d8ba9825c5c'',
		#comment : ''EXPORT VERSION 60184\n'',
		#parentIds : [
			''eed18b164953a5001c2e3a156bfdc670fa6444c6''
		]
	},
	HiExampleCommit {
		#id : ''eed18b164953a5001c2e3a156bfdc670fa6444c6'',
		#comment : ''EXPORT VERSION 60183\n'',
		#parentIds : [
			''41c8605570873420ba5ca0a0a0cd3878e529b8b0''
		]
	},
	HiExampleCommit {
		#id : ''41c8605570873420ba5ca0a0a0cd3878e529b8b0'',
		#comment : ''EXPORT VERSION 60182\n'',
		#parentIds : [
			''1454cec81b0ee1cb725fd4beb45b64836e4b44b2''
		]
	},
	HiExampleCommit {
		#id : ''1454cec81b0ee1cb725fd4beb45b64836e4b44b2'',
		#comment : ''EXPORT VERSION 60181\n'',
		#parentIds : [
			''b07d8e686ce2e0dcfa80fa267e4a39ab79bf743b''
		]
	},
	HiExampleCommit {
		#id : ''b07d8e686ce2e0dcfa80fa267e4a39ab79bf743b'',
		#comment : ''EXPORT VERSION 60180\n'',
		#parentIds : [
			''28fbafbbfcb8f29673124b6d7390f4335f324341''
		]
	},
	HiExampleCommit {
		#id : ''28fbafbbfcb8f29673124b6d7390f4335f324341'',
		#comment : ''EXPORT VERSION 60179\n'',
		#parentIds : [
			''9a4bb66aafbae69542ed9595a91cc74ed9d059e9''
		]
	},
	HiExampleCommit {
		#id : ''9a4bb66aafbae69542ed9595a91cc74ed9d059e9'',
		#comment : ''EXPORT VERSION 60178\n'',
		#parentIds : [
			''d9c68f2cb31b4c657fb23d1358ae1bc81a1f2fd2''
		]
	},
	HiExampleCommit {
		#id : ''d9c68f2cb31b4c657fb23d1358ae1bc81a1f2fd2'',
		#comment : ''EXPORT VERSION 60177\n'',
		#parentIds : [
			''cee0bce0874c7477b0a55a4b9164aaec886b4e9e''
		]
	},
	HiExampleCommit {
		#id : ''cee0bce0874c7477b0a55a4b9164aaec886b4e9e'',
		#comment : ''EXPORT VERSION 60176\n'',
		#parentIds : [
			''8794a4ef59eb07953720bd812232c5826efb42e4''
		]
	},
	HiExampleCommit {
		#id : ''8794a4ef59eb07953720bd812232c5826efb42e4'',
		#comment : ''EXPORT VERSION 60175\n'',
		#parentIds : [
			''29ee9dcb8279de95118ab2fa1f7bdc1330d399ca''
		]
	},
	HiExampleCommit {
		#id : ''29ee9dcb8279de95118ab2fa1f7bdc1330d399ca'',
		#comment : ''EXPORT VERSION 60174\n'',
		#parentIds : [
			''422a14913096e84e9e83b6201a0b1faa75875b4e''
		]
	},
	HiExampleCommit {
		#id : ''422a14913096e84e9e83b6201a0b1faa75875b4e'',
		#comment : ''EXPORT VERSION 60173\n'',
		#parentIds : [
			''56a4f21b19842a2236d8cca67b49cc0a21bf8586''
		]
	},
	HiExampleCommit {
		#id : ''56a4f21b19842a2236d8cca67b49cc0a21bf8586'',
		#comment : ''EXPORT VERSION 60172\n'',
		#parentIds : [
			''28d1cd48ee6c27ba93e98ecff8e88571fac89bf1''
		]
	},
	HiExampleCommit {
		#id : ''28d1cd48ee6c27ba93e98ecff8e88571fac89bf1'',
		#comment : ''EXPORT VERSION 60171\n'',
		#parentIds : [
			''45665d7fcc32d08b55db4bf427e546bd7331817e''
		]
	},
	HiExampleCommit {
		#id : ''45665d7fcc32d08b55db4bf427e546bd7331817e'',
		#comment : ''EXPORT VERSION 60170\n'',
		#parentIds : [
			''9879b988f80855c59251b9f4d3c1691ddb69521e''
		]
	},
	HiExampleCommit {
		#id : ''9879b988f80855c59251b9f4d3c1691ddb69521e'',
		#comment : ''EXPORT VERSION 60169\n'',
		#parentIds : [
			''f4f475042d33597f0dd9ffe7f800096c1075398f''
		]
	},
	HiExampleCommit {
		#id : ''f4f475042d33597f0dd9ffe7f800096c1075398f'',
		#comment : ''EXPORT VERSION 60168\n'',
		#parentIds : [
			''270d99f7c45ec76ed39da9a68f23c113152f9609''
		]
	},
	HiExampleCommit {
		#id : ''270d99f7c45ec76ed39da9a68f23c113152f9609'',
		#comment : ''EXPORT VERSION 60167\n'',
		#parentIds : [
			''e7cd3a1d3e4868e0c492ba3fa4c61da22f018eb6''
		]
	},
	HiExampleCommit {
		#id : ''e7cd3a1d3e4868e0c492ba3fa4c61da22f018eb6'',
		#comment : ''EXPORT VERSION 60166\n'',
		#parentIds : [
			''6ae20d3430bad4c48e9b52776faf1f922457f463''
		]
	},
	HiExampleCommit {
		#id : ''6ae20d3430bad4c48e9b52776faf1f922457f463'',
		#comment : ''EXPORT VERSION 60165\n'',
		#parentIds : [
			''a92e914ce1412c8df6946be3a55137bd05654b58''
		]
	},
	HiExampleCommit {
		#id : ''a92e914ce1412c8df6946be3a55137bd05654b58'',
		#comment : ''EXPORT VERSION 60163\n'',
		#parentIds : [
			''d20d16f8b71df84ece884a179d7a1c4fdbe254a5''
		]
	},
	HiExampleCommit {
		#id : ''d20d16f8b71df84ece884a179d7a1c4fdbe254a5'',
		#comment : ''EXPORT VERSION 60162\n'',
		#parentIds : [
			''f0843cdb6cf507725e7003a31f657a878d0fc0f5''
		]
	},
	HiExampleCommit {
		#id : ''f0843cdb6cf507725e7003a31f657a878d0fc0f5'',
		#comment : ''EXPORT VERSION 60161\n'',
		#parentIds : [
			''c397738876f39cd3b33c6fff2e794253bdb583eb''
		]
	},
	HiExampleCommit {
		#id : ''c397738876f39cd3b33c6fff2e794253bdb583eb'',
		#comment : ''EXPORT VERSION 60160\n'',
		#parentIds : [
			''1fbefa26c0b7640610c971f62e084f88effefbfc''
		]
	},
	HiExampleCommit {
		#id : ''1fbefa26c0b7640610c971f62e084f88effefbfc'',
		#comment : ''EXPORT VERSION 60159\n'',
		#parentIds : [
			''0aba4e1283919b7e7c6120eb4600fae484fbd99f''
		]
	},
	HiExampleCommit {
		#id : ''0aba4e1283919b7e7c6120eb4600fae484fbd99f'',
		#comment : ''EXPORT VERSION 60158\n'',
		#parentIds : [
			''bb179b4905f26351215e406df5994fcfbf273acd''
		]
	},
	HiExampleCommit {
		#id : ''bb179b4905f26351215e406df5994fcfbf273acd'',
		#comment : ''EXPORT VERSION 60157\n'',
		#parentIds : [
			''0756fea9bfad1a66bccbe6e7bd9286d9c0dd391e''
		]
	},
	HiExampleCommit {
		#id : ''0756fea9bfad1a66bccbe6e7bd9286d9c0dd391e'',
		#comment : ''Merge branch \''master\'' of github.com:guillep/pharo-core\n'',
		#parentIds : [
			''26e4836d046495bb78cf3b4185552d956ef0c3bc'',
			''057e1eedda42c04d7c6b143ca14c1e5baae3996c''
		]
	},
	HiExampleCommit {
		#id : ''26e4836d046495bb78cf3b4185552d956ef0c3bc'',
		#comment : ''cleaning travis configuration\n'',
		#parentIds : [
			''9742dc4bb34b0d86b3dd8df245b077be21bd143b''
		]
	},
	HiExampleCommit {
		#id : ''057e1eedda42c04d7c6b143ca14c1e5baae3996c'',
		#comment : ''EXPORT VERSION 60156\n'',
		#parentIds : [
			''9742dc4bb34b0d86b3dd8df245b077be21bd143b''
		]
	},
	HiExampleCommit {
		#id : ''9742dc4bb34b0d86b3dd8df245b077be21bd143b'',
		#comment : ''bootstrapping different architectures\n'',
		#parentIds : [
			''cb2ab02c509afe09691d4d3fc5a13a57999bf5b8''
		]
	},
	HiExampleCommit {
		#id : ''cb2ab02c509afe09691d4d3fc5a13a57999bf5b8'',
		#comment : ''added result upload in travis configuration\n'',
		#parentIds : [
			''1e8a2211546694869ba79254931af3a8cc38c145''
		]
	},
	HiExampleCommit {
		#id : ''1e8a2211546694869ba79254931af3a8cc38c145'',
		#comment : ''added dropbox scripts\n'',
		#parentIds : [
			''07530ee6a97ebb8194cbf21a2751ed4b84022968''
		]
	},
	HiExampleCommit {
		#id : ''07530ee6a97ebb8194cbf21a2751ed4b84022968'',
		#comment : ''quitting after bootstrap\n'',
		#parentIds : [
			''cd638c671a858802994188409992ca7e094f7c9a''
		]
	},
	HiExampleCommit {
		#id : ''cd638c671a858802994188409992ca7e094f7c9a'',
		#comment : ''enhanced bootstrap documentation\n'',
		#parentIds : [
			''2541fd710d37a9cbcd8b0c24287ee1f8983da75d''
		]
	},
	HiExampleCommit {
		#id : ''2541fd710d37a9cbcd8b0c24287ee1f8983da75d'',
		#comment : ''travis yml should invoke pharo vm\n'',
		#parentIds : [
			''2fd2e8764b26f9ebe58747d38d71b72bd62c645d''
		]
	},
	HiExampleCommit {
		#id : ''2fd2e8764b26f9ebe58747d38d71b72bd62c645d'',
		#comment : ''first travis configuration to bootstrap pharo-core\n'',
		#parentIds : [
			''4b531d51ca32cc7cbc54346780e2c6790d879af4''
		]
	},
	HiExampleCommit {
		#id : ''4b531d51ca32cc7cbc54346780e2c6790d879af4'',
		#comment : ''added bootstrap script\n'',
		#parentIds : [
			''5f64f6c0f1588500ab38d01801fd04d9fa70396f''
		]
	},
	HiExampleCommit {
		#id : ''5f64f6c0f1588500ab38d01801fd04d9fa70396f'',
		#comment : ''update gitignore with play iceberg and other caches\n'',
		#parentIds : [
			''6eae964738c7b99ceca9b561bbd94f28ae59bc39''
		]
	},
	HiExampleCommit {
		#id : ''6eae964738c7b99ceca9b561bbd94f28ae59bc39'',
		#comment : ''do not need to remove entire src directory\n'',
		#parentIds : [
			''1e48e645c99d1b15a5980e050f12e2397a1e5523''
		]
	},
	HiExampleCommit {
		#id : ''1e48e645c99d1b15a5980e050f12e2397a1e5523'',
		#comment : ''restored BaselineOfPharoBootstrap.package\n'',
		#parentIds : [
			''3bab5d0efa0e72e81655b84285f1c61a5f160293''
		]
	},
	HiExampleCommit {
		#id : ''3bab5d0efa0e72e81655b84285f1c61a5f160293'',
		#comment : ''exporting removes the package it\''s exporting before writing\n'',
		#parentIds : [
			''7e47731765cfbf6c51e89b59428e9292c21516b4''
		]
	},
	HiExampleCommit {
		#id : ''7e47731765cfbf6c51e89b59428e9292c21516b4'',
		#comment : ''EXPORT VERSION 60155\n'',
		#parentIds : [
			''aa77388990037af5fdf9531b52daf0fae50b36fe''
		]
	},
	HiExampleCommit {
		#id : ''aa77388990037af5fdf9531b52daf0fae50b36fe'',
		#comment : ''git mv moves the correct package\n'',
		#parentIds : [
			''eeb0d5393d8781d250dffea322402f7efb109c1d''
		]
	},
	HiExampleCommit {
		#id : ''eeb0d5393d8781d250dffea322402f7efb109c1d'',
		#comment : ''readded baseline of bootstrap\n'',
		#parentIds : [
			''7f501d81bc670e3f3c5d5955828c23d27c8fa626''
		]
	},
	HiExampleCommit {
		#id : ''7f501d81bc670e3f3c5d5955828c23d27c8fa626'',
		#comment : ''EXPORT VERSION 60155\n'',
		#parentIds : [
			''ef0de0248950b02e47236ac419c64490750997bd''
		]
	},
	HiExampleCommit {
		#id : ''ef0de0248950b02e47236ac419c64490750997bd'',
		#comment : ''put repository url\n'',
		#parentIds : [
			''929341de2db66ff93da3f6b5075116bde5c7f98a''
		]
	},
	HiExampleCommit {
		#id : ''929341de2db66ff93da3f6b5075116bde5c7f98a'',
		#comment : ''removed branch name from commit name\n'',
		#parentIds : [
			''d624b59917433c1fde37db3cfb9f2e276afb5dad''
		]
	},
	HiExampleCommit {
		#id : ''d624b59917433c1fde37db3cfb9f2e276afb5dad'',
		#comment : ''Added statement separator'',
		#parentIds : [
			''3d7bca26f2da9270465168699f26ebaaf185e169''
		]
	},
	HiExampleCommit {
		#id : ''3d7bca26f2da9270465168699f26ebaaf185e169'',
		#comment : ''not losing bootstrap baseline\n'',
		#parentIds : [
			''1096f8cd58ca65ac577966c7dae778cfef00a597''
		]
	},
	HiExampleCommit {
		#id : ''1096f8cd58ca65ac577966c7dae778cfef00a597'',
		#comment : ''exporting PharoBootstrap-Initialization package\n'',
		#parentIds : [
			''382e2c304a558b6af9e16329ce86e9a061dd73a0''
		]
	},
	HiExampleCommit {
		#id : ''382e2c304a558b6af9e16329ce86e9a061dd73a0'',
		#comment : ''--follow-tags does not work in jenkins old git installation\n'',
		#parentIds : [
			''63f504596bb53bf86c1d9eee7e1d873bb8959106''
		]
	},
	HiExampleCommit {
		#id : ''63f504596bb53bf86c1d9eee7e1d873bb8959106'',
		#comment : ''push everything and just follow tags\n'',
		#parentIds : [
			''306bcd373234a1a6ded8472b3b4590e8bc934fc2''
		]
	},
	HiExampleCommit {
		#id : ''306bcd373234a1a6ded8472b3b4590e8bc934fc2'',
		#comment : ''checking out master at the beginning\n'',
		#parentIds : [
			''9b9695e6d9d8db9ddee458752884d56da010de8b''
		]
	},
	HiExampleCommit {
		#id : ''9b9695e6d9d8db9ddee458752884d56da010de8b'',
		#comment : ''added git checkout\n'',
		#parentIds : [
			''f71ef620c02ffaee9df5225de364049c20d230df''
		]
	},
	HiExampleCommit {
		#id : ''f71ef620c02ffaee9df5225de364049c20d230df'',
		#comment : ''fixed commit command\n'',
		#parentIds : [
			''6caeb7b54908a301afb4eaf20ce15adc68b0e7ec''
		]
	},
	HiExampleCommit {
		#id : ''6caeb7b54908a301afb4eaf20ce15adc68b0e7ec'',
		#comment : ''fixed export script syntax errors\n'',
		#parentIds : [
			''844f12f9ad7a019f5b33c22d981548af20999f66''
		]
	},
	HiExampleCommit {
		#id : ''844f12f9ad7a019f5b33c22d981548af20999f66'',
		#comment : ''made all scripts compliant with the new src subdirectory\n'',
		#parentIds : [
			''583d5b95f99f4bb542106ddae5ee08947e3ccf47''
		]
	},
	HiExampleCommit {
		#id : ''583d5b95f99f4bb542106ddae5ee08947e3ccf47'',
		#comment : ''moved all packages to src subdirectory\n'',
		#parentIds : [
			''7081363e3697923426763e17ec5f5bfb5519c425''
		]
	},
	HiExampleCommit {
		#id : ''7081363e3697923426763e17ec5f5bfb5519c425'',
		#comment : ''Importing package BaselineOfPharoBootstrap\n'',
		#parentIds : [
			''553d55e13fb548f812ce852302354ccb05aaffa7''
		]
	},
	HiExampleCommit {
		#id : ''553d55e13fb548f812ce852302354ccb05aaffa7'',
		#comment : ''exporting only Pharo core packages\n'',
		#parentIds : [
			''f6cc3052107fb539ee618b14dafc8847eaf0fdc1''
		]
	},
	HiExampleCommit {
		#id : ''f6cc3052107fb539ee618b14dafc8847eaf0fdc1'',
		#comment : ''should remove all .package directories\n'',
		#parentIds : [
			''ea1dade4e1693a02b45467efb57b21721971ab60''
		]
	},
	HiExampleCommit {
		#id : ''ea1dade4e1693a02b45467efb57b21721971ab60'',
		#comment : ''sorting managers alphabetically\n'',
		#parentIds : [
			''ad7be25f1e128cfaac91edbeebaf158ac1d818f4''
		]
	},
	HiExampleCommit {
		#id : ''ad7be25f1e128cfaac91edbeebaf158ac1d818f4'',
		#comment : ''added explicit url\n'',
		#parentIds : [
			''3ea42f9de4f365c59373c08dfcd95d7d2f5faeeb''
		]
	},
	HiExampleCommit {
		#id : ''3ea42f9de4f365c59373c08dfcd95d7d2f5faeeb'',
		#comment : ''adding debug trace\n'',
		#parentIds : [
			''b084726aac520f0cd8c828ec15a4b3c0ab47b31f''
		]
	},
	HiExampleCommit {
		#id : ''b084726aac520f0cd8c828ec15a4b3c0ab47b31f'',
		#comment : ''fixing typo in export script\n'',
		#parentIds : [
			''8a1f2503b4e54f4737dbd76f733f57aba44bfedc''
		]
	},
	HiExampleCommit {
		#id : ''8a1f2503b4e54f4737dbd76f733f57aba44bfedc'',
		#comment : ''using a normal filetree repository instead of a MCDirectoryRepository\n'',
		#parentIds : [
			''aeff8da8bb4e4754e9bfa901c697df672fe6ba12''
		]
	},
	HiExampleCommit {
		#id : ''aeff8da8bb4e4754e9bfa901c697df672fe6ba12'',
		#comment : ''not using a git repository\n'',
		#parentIds : [
			''dda6b65d9ce0b780a5e9430b133b15d9b5cf55d7''
		]
	},
	HiExampleCommit {
		#id : ''dda6b65d9ce0b780a5e9430b133b15d9b5cf55d7'',
		#comment : ''added save and quit options\n'',
		#parentIds : [
			''a983897c4926492d1f162da91044f3c85b8a2614''
		]
	},
	HiExampleCommit {
		#id : ''a983897c4926492d1f162da91044f3c85b8a2614'',
		#comment : ''added debug messages to export script\n'',
		#parentIds : [
			''7f45f38f46d7d05d392cb7f3221ba7ecf3c3cf21''
		]
	},
	HiExampleCommit {
		#id : ''7f45f38f46d7d05d392cb7f3221ba7ecf3c3cf21'',
		#comment : ''fixed relative path\n'',
		#parentIds : [
			''c29505e3a0846e0232a048e32d17b670f9a9caff''
		]
	},
	HiExampleCommit {
		#id : ''c29505e3a0846e0232a048e32d17b670f9a9caff'',
		#comment : ''gitignoring pharo files, added relative path to bash script\n'',
		#parentIds : [
			''4830e7550dfe8424ad006d3e2eb58ae7798d1420''
		]
	},
	HiExampleCommit {
		#id : ''4830e7550dfe8424ad006d3e2eb58ae7798d1420'',
		#comment : ''added execution permission to export script\n'',
		#parentIds : [
			''68b62987034243eeae199627820672771ad7ded8''
		]
	},
	HiExampleCommit {
		#id : ''68b62987034243eeae199627820672771ad7ded8'',
		#comment : ''added export scripts\n'',
		#parentIds : [
			''ec8d07fc3ade60921057593acf393f08ba3dca7e''
		]
	},
	HiExampleCommit {
		#id : ''ec8d07fc3ade60921057593acf393f08ba3dca7e'',
		#comment : ''initial commit\n'',
		#parentIds : [
			''b3e74a1194ef19408158c741006e956be92ccaa4''
		]
	},
	HiExampleCommit {
		#id : ''b3e74a1194ef19408158c741006e956be92ccaa4'',
		#comment : ''gitfiletree repository properties\n'',
		#parentIds : [
			''32b2dc6616dea41141d80161e3cc68a0b088fb7a''
		]
	},
	HiExampleCommit {
		#id : ''32b2dc6616dea41141d80161e3cc68a0b088fb7a'',
		#comment : ''Initial commit'',
		#parentIds : [ ]
	}
]')
]

{ #category : #examples }
HiExampleCommit class >> randomlyGenerated [

	| values random |
	values := 60 to: 1 by: -1.
	random := Random new.
	^ values collect: [ :value | 
		self new
			id: value;
			comment: 'Value ', value asString;
			parentIds: ((values copyWithout: value) intersection: { random nextInt: values max });
			yourself
		]
]

{ #category : #accessing }
HiExampleCommit >> comment [
	^ comment
]

{ #category : #accessing }
HiExampleCommit >> comment: anObject [
	comment := anObject
]

{ #category : #accessing }
HiExampleCommit >> id [
	^ id
]

{ #category : #accessing }
HiExampleCommit >> id: anObject [
	id := anObject
]

{ #category : #accessing }
HiExampleCommit >> parentIds [
	^ parentIds
]

{ #category : #accessing }
HiExampleCommit >> parentIds: aCollection [
	parentIds := aCollection
]

{ #category : #printing }
HiExampleCommit >> printOn: aStream [
	"Append a sequence of characters to aStream that identify the receiver."

	super printOn: aStream.

	aStream
		nextPut: $(;
		print: id;
		nextPutAll: ' -> ';
		print: parentIds asArray;
		nextPut: $).

]
