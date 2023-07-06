select sub.enterprisenumber, sub.period_begin, c2.employeenumber
from (
	(select b.enterprisenumber, max(period_begin) period_begin
	from branch b 
	inner join contract c 
	on b.enterprisenumber = c.enterprisenumber
	group by b.enterprisenumber
	ORDER BY b.enterprisenumber) as sub
inner join contract c2
	on sub.enterprisenumber=c2.enterprisenumber
and sub.period_begin=c2.period_begin)
order by sub.enterprisenumber
