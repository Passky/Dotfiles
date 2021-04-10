/* Chapter 4 */
create database spj
go

use spj
go

CREATE TABLE s
(
sno char(2)  primary key NOT NULL,
sname varchar(20),
status char(4),
city char(10)
)

CREATE TABLE p
(
pno char(2)  primary key NOT NULL,
pname varchar(20),
color char(4),
weight int
)

CREATE TABLE j
(
jno char(2)  primary key NOT NULL,
jname varchar(20),
city char(10)
)

CREATE TABLE spj
(
sno char(2) not null,
pno char(2) not null,
jno char(2) not null,
qty int
)

-- gcc

-- 3.
use spj
go
create table sum_sno
(
sno char(2) primary key,
psum int
)

insert into sum_sno
select spj.sno,sum(spj.qty)
from spj
group by sno

-- 5.

use spj
go
update spj
set qty=300
where qty in (select top 10 qty from spj)

-- limit 4 // mysql way

-- 10.
use spj
go
alter table spj
add sdate datetime
go
update spj
set sdate = getdate()
/* End of Chapter 4 */
/* Chapter 5 */



alter table Department
add
constraint FK_DepartmentID
primary key(DepartmentID)

CREATE TABLE Course
(Cno Char(4) PRIMARY KEY,
Cname CHAR(40)NOT NULL,
Cpno CHAR(4),
Ceredit SMALLINT
Foreign key (cpno)references Course(cno))

alter table sc
add
constraint FK_cno
foreign key(cno) references course(cno)

use HRM
go
CREATE TABLE Course
(Cno Char(4) PRIMARY KEY,
Cname CHAR(40)NOT NULL,
Cpno CHAR(4),
Ceredit SMALLINT
Foreign key (cpno)references Course(cno))

alter table sc
add
constraint FK_cno
foreign key(cno) references course(cno)


三、 实验过程、步骤及内容

1、 建立课程的实体完整性, 和CourseID cno 的参照完整性;
USE HRM
GO
CREATE TABLE Course
(Cno Char(4) PRIMARY KEY,
Cname CHAR(40)NOT NULL,
Cpno CHAR(4),
Ceredit SMALLINT
Foreign key (cpno)references Course(cno))

alter table sc
add
constraint FK_cno
foreign key(cno) references course(cno)
2、对 HRM 数据库，练习建立三个表的主外键约束、唯一约束、取空值约束、用户自定义
主键约束：
USE HRM
GO
-- Department表：
alter table Department
add
constraint FK_DepartmentID
primary key(DepartmentID)
-- Employee表：
alter table Employee
add
constraint FK_EmployeeID
primary key(EmployeeID)
-- 外键约束：

-- Salary表：
alter table Salary
add
constraint FK_EmployeeID
foreign key(EmployeeID) references Employee(EmployeeID)
-- Employee表：
alter table Employee
add
constraint FK_Employee_Department
foreign key(DepartmentID) references Department(DepartmentID)

-- 唯一约束：
USE HRM
GO
-- Department表：
alter table Department
add
constraint UNIQUE_Department
UNIQUE (DepartmentID)
-- Employee表：
alter table Employee
add
constraint UNIQUE_Employee
UNIQUE (DepartmentID)
-- Salary表：
alter table Salary
add
constraint UNIQUE_Salary
UNIQUE (EmployeeID)
-- 非空约束：
alter table Employee alter column EmployeeID char(6) not null
alter table Employee alter column Name       char(10) not null
alter table Employee alter column Birthday   datetime not null
alter table Employee alter column Sex        bit not null
alter table Employee alter column DepartmentID char(3) not null
-- Department表：
alter table Department alter column DepartmentID char(3) not null
-- Salary表：
alter table Salary alter column EmployeeID char(6) not null
alter table Salary alter column Income   float(8) not null
alter table Salary alter column Outcome   float(8) not null
-- 用户自定义约束：
alter table Employee
add
constraint CK_Sex
check(sex in('男','女'))
3、建立 salary 表的 Income 字段限定在 0-9999 之间。
USE HRM GO
alter table Salary
add
constraint CK_Salary_Income
check(Income between 0 and 9999)



-- 创建角色
create role ProgramerRole
-- 让它拥有创建表，存储过程，视图权限
grant create table,create procedure,create view to ProgramerRole
-- 分配对表Salary的查询、修改、插入的权限
grant select ,insert ,update on Salary to ProgramerRole

-- 创建一个登录账号 Testlogin
create login Testlogin with password = 'Sample';
-- 创建对应于这个登录账号的数据库用户 TestUser
create user TestUser for login Testlogin
-- 将用户 TestUser 添加到 TestRole 角色中
exec sp_addrolemember ProgramerRole, TestUser



1. 启动 SQLServer 查询编辑器，选择要操作数据库，如“sc（学生选课）”数据库。
2. 在查询命令窗口中输入以下 CREATE TRIGGER 语句，创建触发器。
为 sc(学生选课)表创建一个基于 UPDATE 操作和 DELETE 操作的复合型触发器，
3. 当修改grade信息或者删除grade记录时，触发器被激活生效，显示相关的操作信息。
-- 创建触发器
CREATE TRIGGER tri_UPDATE_DELETE_sc
ON sc
FOR UPDATE，DELETE
AS
-- 检测grade列表是否被更新
IF UPDATE(grade)
BEGIN
-- 显示学号、CourseID、原grade和新grade信息
SELECT INSERTED.CourseID，DELETED.grade AS 原grade，
INSERTED.grade AS 新grade
FROM DELETED ，INSERTED
WHERE DELETED.学号=INSERTED.学号
END
-- 检测是更新还是删除操作
ELSE IF COLUMNS_UPDATED( )=0
BEGIN
-- 显示被删除的学号、CourseID和grade信号
SELECT 被删除的学号=DELETED.学号，DELETED.CourseID，
DELETED.grade AS 原grade
FROM DELETED
END
ELSE
--返回提示信息
PRINT ‘ 更新了非grade列！’
③点击快捷工具栏上的快捷按钮，完成触发器的创建。
2.触发触发器
①在查询命令窗口中输入以下 UPDATE sc 语句，修改grade列，激发触发器。
UPDATE sc
SET grade=grade+5
WHERE CourseID=’101’
②在查询命令窗口中输入以下 UPDATE sc 语句修改非grade列，激发触发器。
UPDATE sc
SET CourseID=’113’
WHERE CourseID=’103’
③在查询命令窗口中输入以下 DELETE sc 语句，删除grade记录，激发触发器。
DELETE sc
WHERE CourseID=’102’
3. 比较约束与触发器的不同作用期
①在查询命令窗口中输入并执行以下 ALTER TABLE 语句，为 sc 表添加一个约束，使
得grade只能大于等于 0 且小于等于 100。
ALTER TABLE sc
ADD CONSTRAINT CK_grade
CHECK(grade>=0 AND grade<=100)
②在查询命令窗口中输入并执行以下 UPDATE sc 语句，查看执行结果。
UPDATE sc
SET grade=120
WHERE CourseID=’108’
③在查询命令窗口中输入执行以下 UPDATE sc 语句，查看执行结果。
UPDATE sc
SET grade=90
WHERE CourseID=’108’
从这部分实验中，我们可以看到，约束优先于触发器起作用，它在更新前就
生效，以对要更新的值进行规则检查。当检查到与现有规则冲突时，系统给出
错误消息，并取消更新操作。如果检查没有问题，更新被执行，当执行完毕后，
再激活触发器。
4. 删除新创建的触发器
①在查询命令窗口中输入 DROP TRIGGER 语句，删除新创建的触发器。
DROP TRIGGER tri_UPDATE_DELETE_sc
②点击快捷工具栏上的快捷按钮，删除触发器。


-- 1. 在 Student 表中编写 insert 的触发器，假如每个班的学生不能超过 30 个，如果低于此数，添加可以完成；如果超过此数，则插入将不能实现。
create trigger mytr
on student
for insert, update
as declare @sno numeric
select @sno=sno from student
if(@sno>30)
begin
-- 插入失败
print 'Not done yet!'
rollback
end
else
print 'Successful work!'

-- 2. 在 SC 表上编写 update 触发器，当修改 SC 表中的 grade 字段时将其修改前后的信息保存在 SC_log
create trigger mytr_update
on sc
for update
as
if update(grade)
insert into SC_log select * from deleted
insert into SC_log select * from inserted


-- 创建一个无参存储过程 StuScoreInfo,查询以下信息:学号,姓名,性别,课程名称,考试成绩.
create proceduce StuScoreInfo
as select SC.* , Student.* , Course.*
from SC,Student,Course
where SC.Sno = Student.Sno and
Sc.Cno = Course.Cno

-- 创建一个带参数的存储过程stu_info,该存储过程根据传入的学生编号在student表中查询此学生的信息.
create proceduce stu_info @SNO INT
as select * from Student
where Sno = @SNO

-- 创建一个带参数的存储过程 StuScoreInfo2,该存储过程根据传入的学生编号和课程名称查询以下信息:姓名,课程名称,考试成绩
create proceduce StuScoreInfo2
@Sno as char(9),@Cname as char(40)
as select SC.Sno,
SC.Cno
Sname
Cname
grade
from SC , Student,Course
where SC.Sno = Student.Sno
and SC.Cno = Course.Cno
and Student.Sno = @Sno
and Course.Cname = @Cname

-- 编写带参数的存储过程,根据传入的课程名称统计该课程的平均成绩.
create proceduce Averagemark
@Cname varchar(40)
as select @Cname as '科目', AVG(grade) as 'Average Mark'
from SC,Course where SC.Cno = Course.Cno  and Course.Cname=@Cname

-- 编写存储过程,根据传入的课程名统计这门课的成绩分布情况,即按照各分数段统计人数
create proceduce p_grade @Cname varchar(10)
as select Cname, count(case when grade<60 then l end),
count(case when grade<80 and grade >=60 then l end),
count(case when grade<100 and grade>=80 then l end),
from SC,Course
where sc.Cno=Course.Cno and Cname=@Cname
group by Cname


-- Last Run

-- 1. 针对数据库 stu 创建完全数据库备份集 stu.bak 到c盘根目录
backup database Student
to dist = 'C:\stu.bck'

-- 2. 在数据库 stu 中新建数据表 ceshi ，数据库 stu 创建差异备份；
CREATE TABLE [stu].[ceshi]
(
testID		INT IDENTITY(1,1)		NOT NULL
testname varchar(15)
)
GO
backup database student to
disk = "D:\users\stu_diffe.bak"
with differential

-- 3. 向数据库 stu 的数据表 ceshi 插入部分记录，然后针对数据库 stu 创建事务日志备份；
insert into ceshi
select'0','张A' union
select'1','张B' union
select'2','张C' 
go
backup log student
to dist = 'D:\users\stu_log.bak'

-- 4. 恢复
use master
restore database student
from disk = 'D:\users\stu.bak'

-- 5. 数据库恢复到创建数据表 ceshi 后的状态
restore database student
from disk = 'D:\users\stu_diffe.bak'

-- 6.测试表插入后的状态
restore database student
from disk = 'D:\users\stu_log.bak'





