
select email from
(
	(select max(period_end-period_begin), min(period_begin), license_plate 
	from registration
	group by license_plate) a 
inner join 
	(select license_plate, email, period_end-period_begin as count, period_begin
	from registration) b
on a.license_plate=b.license_plate and a.max=b.count and a.min=b.period_begin
)
