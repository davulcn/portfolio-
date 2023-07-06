
select a.employeenumber, renting_days_during_contract, renting_days_outside_contract from
(
	/***renting in contract***/
	select e.employeenumber,
	sum(r.period_end-r.period_begin+1) as renting_days_during_contract 
	from registration r
	inner join person p 
		on r.email=p.email
	inner join employee e
		on p.email=e.email 
	inner join contract c 
		on e.employeenumber=c.employeenumber
	where	r.period_begin >= c.period_begin and 
			r.period_begin <= c.period_end
	group by e.employeenumber
) a
inner join 
(
	/***renting outside contract**/
	select e.employeenumber,
	sum(r.period_end-r.period_begin+1) as renting_days_outside_contract 
	from registration r
	inner join person p 
		on r.email=p.email
	inner join employee e
		on p.email=e.email 
	inner join contract c 
		on e.employeenumber=c.employeenumber
	where	r.period_begin <= c.period_begin or 
			r.period_begin >= c.period_end
	group by e.employeenumber
) b
on a.employeenumber = b.employeenumber