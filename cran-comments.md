
## Submission summary

This is a small patch release and its main objective is to update some URLs in the DESCRIPTION, README and CITATION files. The older URLs are not working anymore because of a change in my GitHub username.

## Test environments

### Local 

| Platform               | R version | R CMD check | R CMD check --as-cran |
| -----                  | -----     | -----       | -----                 |
| macOS Catalina 10.15.5 | R 4.0.1   | Ok          | 1 NOTE                |
| Ubuntu 20.04 LTS       | R 4.0.1   | Ok          | 1 NOTE                |

* Note: 

```
unable to verify current time
```
After browsing through R CMD check's code I realized that it depends on two external web resources to obtain the current time: http://worldclockapi.org and http://worldclockapi.com/. Both these websites are currently offline and that should be the source of the problem. If that's the case, there is nothing I can do about this note. 

### Win-builder

| R version  | Status |
| -----      | -----  |
| R-oldrel   | 1 NOTE |
| R-release  | 1 NOTE |
| R-devel    | 1 NOTE |

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

## Downstream dependencies

There are currently no downstream dependencies for this package.

