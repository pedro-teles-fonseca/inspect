
## Release summary

* This is the first submission of `inspector`.

## Test environments

### Local 

| Platform               | R version | R CMD check | R CMD check --as-cran |
| -----                  | -----     | -----       | -----                 |
| macOS Catalina 10.15.5 | R 4.0.1   | Ok          | 2 NOTEs               |
| Ubuntu 20.04 LTS       | R 4.0.1   | Ok          | 2 NOTEs               |

* Note 1: `Maintainer: 'Pedro Fonseca <pedro.teles.fonseca@phd.iseg.ulisboa.pt>`
New submission`. This is a standard note for first submissions and should be safe to ignore.

* Note 2: `unable to verify current time`. After browsing through R CMD check's code I realized that it depends on an external web source to check current time (http://worldclockapi.com/). This website is currently offline and that should be the source of the problem. If that's the case, there is nothing I can do about this note. 

### Win-builder

| R version  | Status |
| -----      | -----  |
| R-oldrel   | 1 note |
| R-release  | 1 note |
| R-devel    | 1 note |

* Note: same as note 1 above.

### R-Hub

| Platform                                      | R version | Status |
| -----                                         | -----     | -----  |
| macOS 10.13.6 High Sierra, CRAN's setup       | R-release | Ok     |
| Oracle Solaris 10, x86, 32 bit                | R-release | Ok     |
| Ubuntu Linux 16.04 LTS, GCC                   | R-devel   | Ok     |
| Fedora Linux, GCC                             | R-devel   | Ok     |
| Fedora Linux, clang, gfortran                 | R-devel   | Ok     |
| Debian Linux, GCC                             | R-devel   | Ok     |
| Windows Server 2008 R2 SP1, 32/64 bit         | R-devel   | Ok     |

### GitHub actions

| Platform             | R version                     | Status  |
| -----                | -----                         | -----   |
| macOS Catalina 10.15 | R-oldrel, R-release, R-devel  | Passing |
| Ubuntu 16.04         | R-oldrel, R-release           | Passing |
| Windows Server 2019  | R-oldrel, R-release, R-devel  | Passing |

### Travis CI

| Platform             | R version  | Status |
| -----                | -----      | -----  |
| Linux (Xenial)       | R-oldrel   | Passed |
| Linux (Xenial)       | R-release  | Passed |
| Linux (Xenial)       | R-devel    | Passed |




