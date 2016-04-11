# **Project**
[Programming in Haskell](http://www.fer.unizg.hr/en/course/pih) - Project Assignment UNIZG FER, 2015/2016

### About:
* Few modules of a course management system inspired by [Ferko](http://ferko.fer.hr), but much simpler.
* It will contain most of the functionalities needed for the organization of a course, when combined with other modules.

## Modules implemented:
1. Module for user management
2. Module for email notifications

### 1. User management:
* Uses database written in CSV format.
* Creating, deleting, updating, authenticating users, handling their roles.

### 2. Email notifications:
* Uses custom template language for composing an email
  * TEMPLATE LANGUAGE INSTRUCTIONS:
    * Create your own variables and their values and store them in configuration file
    * Example: {firstName} -> David, {authorName} -> Joseph, *variable* -> True, something -> somethingelse
    * Use if statements with basic boolean functions
    * <i>Arguments can be variables or even functions so this will work too:</i> <b>@if(And_(Or_True_*variable*)_False)</b>

| Operator | Function      |
| ---      | ------------- |
| or       | Or_Arg1_Arg2  |
| not      | And_Arg1_Arg2 |
| and      | Not_Arg1      |

```
Hello {firstName}

This is an example of “if statement”:
@if(And_(Or_True_*variable*)_False)

Condition passed!
Message number one.

*variable*

@else
Condition failed!

Message number two.
something

@endif

Regards,
{authorName}
```

* Uses configuration for an email
  * CONFIGURATION FILE INSTRUCTIONS:
    * File name : "email.config"
    * Must consist of 6 lines in this order:
    * <i>Each line is written as you normally would in Haskell</i>
    * <i>Hostname, Port, Sender, Username and Password values are written without <b>quotation marks</b></i>

| No. | Line          |
| --- | ------------- |
| 1.  | Hostname      |
| 2.  | Port          |
| 3.  | Sender        |
| 4.  | Username      |
| 5.  | Password      |
| 6.  | Map           |

```
hostname = smtp.host.name
port = 25
sender = john.doe@example.com
username = John123
password = p2sw0rd
map = [("{firstName}","David"),("{authorName}","John"),("something","somethingelse")]
```

* Reading configuration, compiling template email, sending email

