let navLinks = document.querySelectorAll('a.inner-link');

navLinks.forEach((item) => {
    item.addEventListener('click', function (e) {
        e.preventDefault();
        document.querySelector('nav ul li a.active').classList.remove('active');
        document.querySelector(`nav ul li a[href='${item.getAttribute('href')}']`).classList.add('active');
        document.querySelector('main > section.active').classList.remove('active');
        document.querySelector(`main > section${item.getAttribute('href')}`).classList.add('active');
    });
});

document.querySelector('#sidebar .toggle-sidebar').addEventListener('click', function () {
    document.querySelector('#sidebar').classList.toggle('open');
});

var options = {
    strings: ['Master student in M.Sc. Digital Media'],
    loop: true,
    typeSpeed: 70,
    backSpeed: 10
};

// Only run Typed if the element exists
if (document.querySelector('.field h2')) {
    new Typed('.field h2', options);
}

// Meteor shower (only if the container exists)
const meteorContainer = document.querySelector('#home .meteor-shower');
if (meteorContainer) {
    for (let i = 1; i <= 15; i++) {
        let meteor = document.createElement('span');
        meteor.classList = 'meteor';
        meteorContainer.append(meteor);
    }
}

const shuffleInstance = new Shuffle(document.querySelector('#my_work .work-items'), {
    itemSelector: '.item'
});

const filterButtons = document.querySelectorAll('#my_work .filters button');

filterButtons.forEach((item) => {
    item.addEventListener('click', workFilter);
});

function workFilter(event) {
    const clickedButton = event.currentTarget;
    const clickedButtonGroup = clickedButton.getAttribute('data-group');
    const activeButton = document.querySelector('#my_work .filters button.active');

    activeButton.classList.remove('active');
    clickedButton.classList.add("active");

    shuffleInstance.filter(clickedButtonGroup);
}

var workModal = new bootstrap.Modal(document.getElementById('workModal'));
const workElements = document.querySelectorAll("#my_work .work-items .wrap");

workElements.forEach((item) => {
    item.addEventListener('click', function () {
        document.querySelector('#workModal .modal-body img').setAttribute('src', item.getAttribute('data-image'));
        document.querySelector('#workModal .modal-body .title').innerText = item.getAttribute('data-title');
        document.querySelector('#workModal .modal-body .description').innerText = item.getAttribute('data-description');
        document.querySelector('#workModal .modal-body .client .value').innerText = item.getAttribute('data-client');
        document.querySelector('#workModal .modal-body .completed .value').innerText = item.getAttribute('data-completed');
        document.querySelector('#workModal .modal-body .skills .value').innerText = item.getAttribute('data-skills');
        document.querySelector('#workModal .modal-body .project-link a').setAttribute('href', item.getAttribute('data-project-link'));

        workModal.show();
    });
});

var workModalElement = document.getElementById('workModal');
workModalElement.addEventListener('show.bs.modal', function (event) {
    document.getElementById('my_work').classList.add('blur');
    document.getElementById('sidebar').classList.add('blur');
});

workModalElement.addEventListener('hide.bs.modal', function (event) {
    document.getElementById('my_work').classList.remove('blur');
    document.getElementById('sidebar').classList.remove('blur');
});

let testimonialImages = document.querySelectorAll('#testimonial .images img');

testimonialImages.forEach((item, index) => {
    let position = index + 1;

    item.addEventListener('click', function () {
        document.querySelector('#testimonial .images img.active').classList.remove('active');
        document.querySelector(`#testimonial .images img:nth-child(${position})`).classList.add('active');

        document.querySelector('#testimonial .comments .item.active').classList.remove('active');
        document.querySelector(`#testimonial .comments .item:nth-child(${position})`).classList.add('active');
    });
});

let contactFromItems = document.querySelectorAll('#contact_me .form input, #contact_me .form textarea');

contactFromItems.forEach((item) => {
    item.addEventListener('focus', function () {
        item.parentElement.classList.add('focus');
    });

    item.addEventListener('blur', function () {
        if (!item.value) {
            item.parentElement.classList.remove('focus');
        }
    });
});

// ===== Scroll-based section switching =====
const sections = document.querySelectorAll('main > section');
const navLinksScroll = document.querySelectorAll('a.inner-link');

const observerOptions = {
  root: null,
  rootMargin: '-40% 0px -40% 0px', // triggers when section is roughly in the middle
  threshold: 0
};

const sectionObserver = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    if (entry.isIntersecting) {
      const id = entry.target.id;
      // Update active nav link
      navLinksScroll.forEach(link => {
        link.classList.remove('active');
        if (link.getAttribute('href') === `#${id}`) {
          link.classList.add('active');
        }
      });
      // Update active section class
      sections.forEach(sec => sec.classList.remove('active'));
      entry.target.classList.add('active');
    }
  });
}, observerOptions);

sections.forEach(section => {
  sectionObserver.observe(section);
});

// Improve click navigation: scroll to section instead of only toggling class
navLinks.forEach((item) => {
  item.addEventListener('click', function (e) {
    e.preventDefault();
    const targetId = this.getAttribute('href');
    const targetSection = document.querySelector(targetId);
    if (targetSection) {
      targetSection.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  });
});
