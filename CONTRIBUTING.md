# How to contribute

First and foremost, sign the [license agreement](http://files.pharo.org/media/PharoSoftwareDistributionAgreement.pdf).

## Reporting issues
If you found a bug or issue with Pharo please let us know. You can report bugs on the issue tracker.

### How to report a bug
*  Take the latest stable or latest development system, ideally using the latest virtual machine is better (check [download](https://pharo.org/download) page).
*  Make sure that you can reproduce the bug.
*  Send a mail to the mailing-list pharo-dev mailing if you are not sure, check if the bug was already reported on https://github.com/pharo-project/pharo/issues
*  Open a bug entry on https://github.com/pharo-project/pharo/issues
*  Do not forget to mention the virtual machine version, your OS and other crucial information
*  Follow your bug. Indeed it may happen that your bug is fixed by introducing another bug fix and you can verify and close it.
*  Watch the video from the [Pharo TechTalk February 2018](https://www.youtube.com/watch?v=VW7XrFjnbyw) for a step by step tutorial.

### How to propose a bug fix or enhancement

First check if your change has already been entered into the [issue tracker](https://github.com/pharo-project/pharo/issues), and consider watching the [overview video](https://vimeo.com/162493974) of the steps documented below. 

#### Pharo 70 and git

Since Pharo 70 alpha, Pharo is managed with git and hosted on github. To submit a fix we suggest:

- make sure that you use the latest image
- do a Pull request in the development branch. For a full description, see [here](https://github.com/pharo-project/pharo/wiki/Contribute-a-fix-to-Pharo).
- Watch the video from the [Pharo TechTalk February 2018](https://www.youtube.com/watch?v=VW7XrFjnbyw) for a step by step tutorial.

**_IMPORTANT NOTICE_**: A new version of [Iceberg](https://github.com/pharo-vcs/iceberg) (the VCS git tool used in Pharo) is being developed to improve our workflow. Pharo Images downloaded since May/2018 contain this newer version and in this case please follow these slightly different instructions:
- rebind your repository to a local clone fork, make your changes, commit, push, and then make a Pull request. For a full description, read [this description](https://github.com/pharo-vcs/iceberg/wiki/Contribute-to-Pharo-with-Iceberg-0.7.3).
- Watch the video from the [Pharo TechTalk February 2018](https://www.youtube.com/watch?v=PK2yCu2rWCc) for a step by step tutorial.

#### Old Procedure (for pre git for Pharo versions: 40, 50, 60)

You also need an account on [SmalltalkHub](http://smalltalkhub.com/) with commit rights to the [Pharo Inbox](http://smalltalkhub.com/#!/~Pharo/Pharo60Inbox). _Note, in the recent version of SmalltalkHub this is a public repository. If you encounter problems you can ask for commit rights on the *Pharo Developers mailinglist&gt;http://lists.pharo.org/mailman/listinfo/pharo-dev_lists.pharo.org*._

In summary you will do the following:

- Take the latest stable or latest development system, ideally using the latest virtual machine (check the [downloads](https://pharo.org/download)).
- Do a fix for a given bug, or a new bug you entered on the [issue tracker](https://github.com/pharo-project/pharo/issues).
- Create a code fix (ideally with a unit test exercising the working code)
- Create a slice (group of packages):
    - Open the Monticello Browser.
    - Open the Slice Maker by clicking the "Slice" button.
    - In Slice Maker enter the Fogbugz/Manuscript issue number and "grab" the issue summary.
    - Select the dirty packages relevant to your bug fix. Dirty packages are prefixed by a start and are sorted to the top of the list of packages.

- Verify your changes (click on changes button)
    - Select the Pharo60 repository
    - Select your slice
    - Press changes in the Monticello Browser and verify that your changes are the correct one.

- Publish the slice to Pharo60Inbox:
    - Select the whole slice (not it's individual packages).
    - Select the SmalltalkHub Pharo Inbox repository.
    - Press the "Save" button to commit your slice to the inbox repository

- Verify that your fix loads: take a fresh image and load the slice you published.
- Update the bug entry to mention that you submitted a slice in the inbox.
- In Fogzbugz, click on "Resolve" to Change the state of the issue to "Resolved: Fix Review Needed" and ensure that the milestone is set to the next release of Pharo.
- Now you can feel good!

#### Monkey checking your submission

The fix then will be checked by an automated tool (the monkey). If this test fails, the state of your issue reverts to "Work needed". In success, the state will be "Fix Reviewed by the Monkey". This is where humans come into the picture: your code needs to be reviewed, if possible by 2-3 people. If they accept it, the state moves to "Fix to Include", after which it will be committed to the development branch as soon as possible.



Guide taken from https://pharo.org/contribute
