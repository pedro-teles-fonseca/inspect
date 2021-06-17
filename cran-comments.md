
## Submission summary

This is a small patch release to fix the following note from CRANs checks:
```
checking LazyData ... NOTE
'LazyData' is specified without a 'data' directory
```

## Test environments

### Local 

| Platform               | R version | R CMD check | R CMD check --as-cran |
| -----                  | -----     | -----       | -----                 |
| macOS Catalina 11.3.0  | R 4.1.0   | Ok          | Ok                    |
| Ubuntu 20.04 LTS       | R 4.0.2   | Ok          | Ok                    |

### Win-builder

| R version  | Status |
| -----      | -----  |
| R-oldrel   | OK     |
| R-release  | OK     |
| R-devel    | OK     |

### R-Hub

| Platform                                      | R version | Status |
| -----                                         | -----     | -----  |
| macOS 10.13.6 High Sierra, CRAN's setup       | R-release | Ok     |
| Oracle Solaris 10, x86, 32 bit                | R-release | Ok     |
| Ubuntu Linux 20.04 LTS, GCC                   | R-devel   | Ok     |
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

