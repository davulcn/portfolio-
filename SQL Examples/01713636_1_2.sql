SELECT DISTINCT brand
FROM carmodel
WHERE brand NOT IN
(
	SELECT DISTINCT brand
	FROM carmodel cm1
	WHERE NOT EXISTS(
		SELECT 1
		FROM carmodel cm2
		WHERE cm1.brand = cm2.brand
		AND cm1.type != cm2.type
	)

)

select count(*) from carmodel
select 1 from carmodel